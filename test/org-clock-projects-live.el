;;; org-clock-projects-live.el --- Live export check for org-clock-projects -*- lexical-binding: t; -*-

;; Runs the exporter and the check command against the real registry and the
;; real Org corpus, through the bootstrap harness.  The package's own suite
;; proves the logic against fixtures and `org-clock-projects-smoke.el' proves
;; the wiring, but neither reads a byte of the data that is actually invoiced,
;; so a parse that disagrees with the corpus — an archive left out of the scan,
;; a clock line shaped in a way no fixture carries — passes both and reaches
;; the invoice.
;;
;; Every count here is checked against an oracle built by plain text search
;; over the same files, so the assertion is between two independent readings of
;; the corpus rather than between the package and itself.  A month of real
;; data is the fixture, and the previous month is chosen because it is the one
;; that is finished and the one that gets invoiced.
;;
;; Nothing is written outside a temporary directory: the invoice build's own
;; CSVs are never touched.  Run it with:
;;
;;   emacs -q --batch -l ~/.config/doom/test/bootstrap.el \\
;;         -l ~/.config/doom/test/org-clock-projects-live.el

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-clock)
(require 'agile-gtd)
(require 'org-clock-projects)

(defvar opc-live-failures 0)

(defun opc-live-check (label ok &optional detail)
  "Report LABEL as passing when OK, with DETAIL beside it either way."
  (message "%s %-56s %s" (if ok "PASS" "FAIL") label (or detail ""))
  (unless ok (setq opc-live-failures (1+ opc-live-failures))))

(defmacro opc-live-checking (label &rest body)
  "Report LABEL as failing when BODY signals, rather than aborting the run.
One broken seam would otherwise take every check below it with it, and a
live run is exactly where an unforeseen shape of data shows up."
  (declare (indent 1))
  `(condition-case err (progn ,@body)
     (error (opc-live-check ,label nil (error-message-string err)))))


;;;; The project and the period under test

(defconst opc-live-tag "oebb"
  "The project the corpus holds the most clocked time for.")

(defvar opc-live-record
  (seq-find (lambda (record) (equal (plist-get record :tag) opc-live-tag))
            (funcall org-clock-projects-projects-function))
  "The registry record the export is asked for, as the package resolves it.")

(defun opc-live-previous-month ()
  "Return the year and month of the month before this one."
  (let* ((now (decode-time))
         (month (1- (nth 4 now)))
         (year (nth 5 now)))
    (if (zerop month) (list (1- year) 12) (list year month))))

(defconst opc-live-period
  (pcase-let* ((`(,year ,month) (opc-live-previous-month))
               (start (encode-time 0 0 0 1 month year))
               (end (encode-time 0 0 0 1 (1+ month) year)))
    (list :label (format "%04d-%02d" year month) :start start :end end))
  "The month the export is asked for, resolved here rather than by the package.")

(defconst opc-live-label (plist-get opc-live-period :label))

(defun opc-live-stamp (time)
  "Return TIME as the `YYYY-MM-DD HH:MM' string a clock line carries."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defconst opc-live-from (opc-live-stamp (plist-get opc-live-period :start)))
(defconst opc-live-until (opc-live-stamp (plist-get opc-live-period :end)))


;;;; The oracle
;;
;; A second reading of the same files, by text search rather than by
;; `org-element', so that the two can be compared.  It knows where the project
;; files are from the registry's file name and Org's archive convention, not
;; from the package's own scope derivation, which is half of what it checks.

(defconst opc-live-files
  (seq-filter #'file-exists-p
              (list (expand-file-name (plist-get opc-live-record :file) org-directory)
                    (expand-file-name (concat "archive/" (plist-get opc-live-record :file))
                                      org-directory)))
  "The project's own file and its datetree archive, as the corpus has them.")

(defconst opc-live-clock-regexp
  "^[ \t]*CLOCK: \\[\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\) [^]0-9]*\\([0-9][0-9]:[0-9][0-9]\\)\\]\\(--\\[\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\) [^]0-9]*\\([0-9][0-9]:[0-9][0-9]\\)\\]\\)?"
  "A CLOCK line, closed or still running, with its dates and times apart.")

(defun opc-live-clock-lines ()
  "Return every CLOCK line in the project's files as (START . END) stamps.
END is nil for a clock that is still running.  Read from the file text
with no Org parsing in the way."
  (let (lines)
    (dolist (file opc-live-files (nreverse lines))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward opc-live-clock-regexp nil t)
          (push (cons (format "%s %s" (match-string 1) (match-string 2))
                      (and (match-string 3)
                           (format "%s %s" (match-string 4) (match-string 5))))
                lines))))))

(defconst opc-live-clocks (opc-live-clock-lines))

(defun opc-live-in-period-p (clock)
  "Return non-nil when CLOCK covers any instant of the period.
Strictly at both ends, so an entry touching a boundary belongs to the
period it lies inside and not to its neighbour."
  (and (cdr clock)
       (string< (car clock) opc-live-until)
       (string< opc-live-from (cdr clock))))

(defconst opc-live-expected
  (sort (seq-filter #'opc-live-in-period-p opc-live-clocks)
        (lambda (a b) (string< (car a) (car b))))
  "The clock lines the export is expected to carry, oldest first.")

(defconst opc-live-straddlers
  (seq-filter (lambda (clock)
                (or (string< (car clock) opc-live-from)
                    (string< opc-live-until (cdr clock))))
              opc-live-expected)
  "Selected entries reaching outside the period, which are exported whole.")

(defconst opc-live-open
  (seq-filter (lambda (clock)
                (and (null (cdr clock)) (string< (car clock) opc-live-until)))
              opc-live-clocks)
  "Clocks still running that hold time the period would have wanted.")

(defun opc-live-minutes (clock)
  "Return the whole minutes CLOCK ran for."
  (round (/ (float-time (time-subtract (org-time-string-to-time (cdr clock))
                                       (org-time-string-to-time (car clock))))
            60)))


;;;; Driving the commands

(defvar opc-live-answers nil
  "Answers the next command's prompts are given, in the order they are asked.")

(defvar opc-live-asked nil
  "The prompts a command actually asked, newest last.")

(defun opc-live-completing-read (prompt &rest _)
  "Answer PROMPT from `opc-live-answers'.
A prompt the queue did not anticipate is itself a failure: a command that
asks something unforeseen has changed, and answering it by default would
hide that."
  (push prompt opc-live-asked)
  (unless opc-live-answers
    (error "Unanticipated prompt: %s" prompt))
  (pop opc-live-answers))

(defmacro opc-live-answering (answers &rest body)
  "Run BODY with the completion prompts answered by ANSWERS in turn."
  (declare (indent 1))
  `(let ((opc-live-answers ,answers)
         (opc-live-asked nil))
     (cl-letf (((symbol-function 'completing-read) #'opc-live-completing-read))
       ,@body)))

(defconst opc-live-directory
  (file-name-as-directory (make-temp-file "org-clock-projects-live-" t))
  "Where the exports under test are written, so the invoice build keeps its own.")

(defun opc-live-export (period &optional arg)
  "Export the project for PERIOD under prefix ARG and return the file's lines."
  (let ((org-clock-projects-export-directory opc-live-directory))
    (opc-live-answering (list (plist-get opc-live-record :name) period)
      (org-clock-projects-export arg))))

(defun opc-live-path ()
  "Return the file the period's export is expected to land at."
  (expand-file-name (format "%s-org-clock-%s.csv" opc-live-label
                            (file-name-base (plist-get opc-live-record :file)))
                    opc-live-directory))

(defun opc-live-rows (path)
  "Return PATH's data lines, the header dropped."
  (with-temp-buffer
    (insert-file-contents path)
    (cdr (split-string (buffer-string) "\n" t))))

(defconst opc-live-row-times
  ",\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\),\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\),"
  "A row's start and end columns, which are the only adjacent pair of stamps.")

(defun opc-live-row-clock (row)
  "Return ROW's (START . END) stamps."
  (and (string-match opc-live-row-times row)
       (cons (match-string 1 row) (match-string 2 row))))


;;;; What the corpus holds

(opc-live-check "the project is registered" (and opc-live-record t)
                (format "%S" opc-live-record))
(opc-live-check "its files are on disk" (= (length opc-live-files) 2)
                (mapconcat #'abbreviate-file-name opc-live-files " "))
(opc-live-check "the period has clocked time in it" (> (length opc-live-expected) 0)
                (format "%s: %d of %d clock lines"
                        opc-live-label (length opc-live-expected)
                        (length opc-live-clocks)))


;;;; The export

(defvar opc-live-narrow nil "The rows the ordinary export wrote.")

(opc-live-checking "the export writes the period's file"
  ;; Checks waived, because whether a clock is open right now is a property of
  ;; the moment this runs and not of the exporter.  What the checks would have
  ;; said is asserted through the check command below, where it is the subject.
  (opc-live-export opc-live-label '(16))
  (let ((path (opc-live-path)))
    (opc-live-check "named for the period and the project file"
                    (file-exists-p path) (file-name-nondirectory path))
    (when (file-exists-p path)
      (setq opc-live-narrow (opc-live-rows path))
      (with-temp-buffer
        (insert-file-contents path)
        (opc-live-check "headed by the configured columns"
                        (equal (car (split-string (buffer-string) "\n"))
                               org-clock-csv-header)))
      (let ((clocks (mapcar #'opc-live-row-clock opc-live-narrow)))
        (opc-live-check "every row carries a start and an end"
                        (not (memq nil clocks))
                        (format "%d rows" (length opc-live-narrow)))
        (opc-live-check "as many rows as the corpus has clock lines in the period"
                        (= (length opc-live-narrow) (length opc-live-expected))
                        (format "%d exported, %d found"
                                (length opc-live-narrow) (length opc-live-expected)))
        (opc-live-check "the same clock lines the corpus holds"
                        (equal clocks opc-live-expected))
        (opc-live-check "oldest first"
                        (equal clocks (sort (copy-sequence clocks)
                                            (lambda (a b) (string< (car a) (car b))))))
        (opc-live-check "no row lies wholly outside the period"
                        (seq-every-p #'opc-live-in-period-p (delq nil clocks)))))))

;; The relative key is the one the prompt defaults to, and it reaches the
;; resolver as a string where the resolver matches symbols, so it is the form
;; that fails while every typed form works.
(opc-live-checking "the relative key names the same month"
  (opc-live-export "lastmonth" '(16))
  (opc-live-check "lastmonth resolves to the previous month"
                  (file-exists-p (opc-live-path))))

(opc-live-checking "a bare day is refused"
  (let ((refused (condition-case err
                     (progn (opc-live-export
                             (format "%s-15" opc-live-label) '(16))
                            nil)
                   (user-error (error-message-string err)))))
    (opc-live-check "a single date does not export as a month"
                    (and refused (string-match-p "single day" refused))
                    (or refused "no error"))))

;; The wide scan is the audit path: the tag is the only selector and every
;; agenda file is read, so it can only ever find more.
(opc-live-checking "the wide scan holds everything the narrow one did"
  (opc-live-export opc-live-label '(64))
  (let ((wide (opc-live-rows (opc-live-path))))
    (opc-live-check "the narrow rows are a subset of the wide ones"
                    (seq-every-p (lambda (row) (member row wide)) opc-live-narrow)
                    (format "%d wide, %d narrow" (length wide) (length opc-live-narrow)))))


;;;; The golden sum
;;
;; Org's own clocktable over the same range, which clips an entry at the
;; period's edge where the export writes it whole.  The two therefore agree
;; only while nothing straddles a boundary, which the corpus is asserted to
;; satisfy rather than assumed to.

(opc-live-checking "the export sums to what Org's clocktable does"
  (opc-live-check "no selected entry crosses the period's edge"
                  (null opc-live-straddlers)
                  (format "%d straddler(s)" (length opc-live-straddlers)))
  (if opc-live-straddlers
      (message "SKIP %-56s %s" "clocktable comparison"
               "a straddler is exported whole and clocked clipped")
    (let ((exported (apply #'+ (mapcar #'opc-live-minutes opc-live-expected)))
          (clocked 0))
      (dolist (file opc-live-files)
        (with-current-buffer (find-file-noselect file)
          (cl-incf clocked
                   ;; The file name, the file's total minutes, then its
                   ;; headlines.  The total is what a clocktable prints as the
                   ;; file line, over the range the same period fixed.
                   (or (nth 1 (org-clock-get-table-data
                               file (list :maxlevel 99
                                          :tstart opc-live-from
                                          :tend opc-live-until)))
                       0))))
      (opc-live-check "the same minutes as Org counts"
                      (= exported clocked)
                      (format "%d exported, %d clocked" exported clocked)))))


;;;; The check command

(opc-live-checking "the check reports on the same project and period"
  (opc-live-answering (list (plist-get opc-live-record :name) opc-live-label)
    (save-window-excursion (org-clock-projects-check)))
  (with-current-buffer (get-buffer "*org-clock-projects-check*")
    (let ((text (buffer-string)))
      (opc-live-check "the report names the project and the period"
                      (string-prefix-p (format "%s over %s: "
                                               (plist-get opc-live-record :name)
                                               opc-live-label)
                                       text)
                      (car (split-string text "\n")))
      ;; The oracle knows whether a clock is open, so the report has to agree
      ;; with it either way — an empty report is only correct when there is
      ;; nothing to say.
      (opc-live-check "open clocks are reported exactly when the corpus has them"
                      (eq (null opc-live-open)
                          (null (string-match-p "^open-clock " text)))
                      (format "%d open in the corpus" (length opc-live-open))))))

(message "\n=== %d failure(s) ===" opc-live-failures)
(delete-directory opc-live-directory t)
(kill-emacs (min opc-live-failures 1))

(provide 'org-clock-projects-live)
;;; org-clock-projects-live.el ends here
