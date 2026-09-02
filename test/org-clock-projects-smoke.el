;;; org-clock-projects-smoke.el --- Wiring check for org-clock-projects -*- lexical-binding: t; -*-

;; The package suite cannot catch wiring errors and the bootstrap run cannot
;; catch logic errors, so this checks only that the configuration hands the
;; package what it expects.  Run it with:
;;
;;   emacs -q --batch -l ~/.config/doom/test/bootstrap.el \\
;;         -l ~/.config/doom/test/org-clock-projects-smoke.el

;;; Code:
(require 'org)
(require 'org-clock)
(require 'agile-gtd)

(defvar opc-smoke-failures 0)
(defun opc-check (label ok &optional detail)
  (message "%s %-52s %s" (if ok "PASS" "FAIL") label (or detail ""))
  (unless ok (setq opc-smoke-failures (1+ opc-smoke-failures))))

(opc-check "package loads" (featurep 'org-clock-projects))
(opc-check "mode is on" (bound-and-true-p org-clock-projects-mode))

;; Every symbol the configuration hands the package is read through
;; `bound-and-true-p', so a package build without it reports one failed line
;; instead of aborting the run and hiding every check below it.
(opc-check "resolver wired to agile-gtd"
           (eq (bound-and-true-p org-clock-projects-projects-function)
               #'agile-gtd-project-records))

(defvar opc-records
  (and (functionp (bound-and-true-p org-clock-projects-projects-function))
       (funcall org-clock-projects-projects-function))
  "The registry as the package sees it through the configured resolver.")

(opc-check "resolver returns the real project list"
           (and (= (length opc-records) (length agile-gtd-projects))
                (seq-find (lambda (r) (equal (plist-get r :file) "oebb.org")) opc-records))
           (format "%d records" (length opc-records)))

;; Café Glas is the project whose tag, name and file base all differ — the case
;; that tells a record apart from the bare file name it used to be.
(let ((glas (seq-find (lambda (r) (equal (plist-get r :tag) "glas")) opc-records)))
  (opc-check "records carry tag, name and file separately"
             (and (equal (plist-get glas :name) "Café Glas")
                  (equal (plist-get glas :file) "cafe-glas.org"))
             (format "%S" glas)))

;; Relative names from the registry must resolve against org-directory.
(let ((expanded (mapcar (lambda (r)
                          (file-truename
                           (expand-file-name (plist-get r :file) org-directory)))
                        opc-records)))
  (opc-check "project files exist on disk"
             (and expanded (seq-every-p #'file-exists-p expanded))
             (format "%d/%d" (seq-count #'file-exists-p expanded) (length expanded))))

(opc-check "export directory points at the invoice build"
           (equal (bound-and-true-p org-clock-projects-export-directory)
                  "~/work/invoice.typ/invoices")
           (format "%s" (bound-and-true-p org-clock-projects-export-directory)))

;; The exporter writes its header and rows through these, so the CSV keeps the
;; columns the invoice build reads.
(opc-check "CSV rows come from the configured formatter"
           (and (eq (bound-and-true-p org-clock-csv-row-fmt)
                    #'stfl/org-clock-csv-row-fmt)
                (string-prefix-p "task,parents,archive_parents"
                                 (or (bound-and-true-p org-clock-csv-header) ""))))

;; The matrix belongs to the clock package; two definitions of the dynamic
;; block would resolve by load order and render a plausible but wrong table.
(opc-check "clockmatrix lives in org-clock-projects"
           (and (fboundp 'org-clock-projects-clockmatrix)
                (not (fboundp 'agile-gtd-clockmatrix))))

;; A key binds to its symbol whether or not anything defines it, so the
;; bindings below only say the keys are free; this says the commands exist.
(dolist (cmd '(org-clock-projects-export org-clock-projects-check))
  (opc-check (format "%s is a command" cmd) (commandp cmd)))

(opc-check "auto clock resolution disabled" (null org-clock-auto-clock-resolution))

(opc-check "modeline segment installed"
           (memq 'org-clock-projects-mode-line-string global-mode-string))
(opc-check "modeline segment empty while dormant"
           (equal org-clock-projects-mode-line-string ""))

(opc-check "exit query replaced"
           (and (memq #'org-clock-projects-kill-emacs-query kill-emacs-query-functions)
                (not (memq #'org-clock-kill-emacs-query kill-emacs-query-functions))))

(opc-check "clock-in advice installed"
           (advice-member-p #'org-clock-projects--clock-in-advice 'org-clock-in))
(opc-check "clock-out advice installed"
           (advice-member-p #'org-clock-projects--clock-out-advice 'org-clock-out))

;; The advice and the exporter the package absorbed must be gone from the config.
(dolist (sym '(stfl/org-clock-continue? stfl/org-clock-continous-threshold
               stfl/org-time-minutes-ago stfl/org-time-minutes-ago-rounded
               stfl/org-time-format-ago
               stfl/org-clock-export stfl/org-clock-export-dir))
  (opc-check (format "%s no longer defined" sym)
             (not (or (fboundp sym) (boundp sym)))))
;; ...while the ones that stayed behind still work.
(dolist (sym '(stfl/org-clock-in-at stfl/org-clock-out-at stfl/org-read-date-time))
  (opc-check (format "%s still defined" sym) (fboundp sym)))

(with-temp-buffer
  (org-mode)
  (dolist (spec '(("SPC m c p" org-clock-projects-fork)
                  ("SPC m c a" org-clock-projects-clock-out-all)
                  ("SPC m c P" org-clock-projects-clear)
                  ("SPC m c I" stfl/org-clock-in-at)
                  ("SPC m c O" stfl/org-clock-out-at)
                  ("SPC n o"   org-clock-projects-switch)
                  ("SPC n e"   org-clock-projects-check)
                  ("SPC n E"   org-clock-projects-export)))
    (let ((bound (key-binding (kbd (car spec)))))
      (opc-check (format "%s -> %s" (car spec) (cadr spec))
                 (eq bound (cadr spec))
                 (format "got %s" bound)))))

(message "\n=== %d failure(s) ===" opc-smoke-failures)
;; Exit non-zero so a failure is visible to something other than a human
;; reading the output.
(kill-emacs (min opc-smoke-failures 1))

(provide 'org-clock-projects-smoke)
;;; org-clock-projects-smoke.el ends here
