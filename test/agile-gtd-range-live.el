;;; agile-gtd-range-live.el --- View-range checks against the real Org corpus -*- lexical-binding: t; -*-

;; The package's own suite proves the range rules on a fixture it writes.
;; This proves them on the corpus they were written for, which is the only
;; place a rule meets an entry nobody thought to invent: a bare action three
;; levels under a low project, a monthly tickler with a deadline behind it, a
;; habit that repeats from completion.  A range that holds here holds.
;;
;; Read-only.  It runs queries and renders an agenda; it writes nothing.
;;
;;   emacs -q --batch -l ~/.config/doom/test/bootstrap.el \\
;;         -l ~/.config/doom/test/agile-gtd-range-live.el

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'org-ql)
(require 'org-super-agenda)
(require 'agile-gtd)

(defvar agr-failures 0)
(defun agr-check (label ok &optional detail)
  (message "%s %-58s %s" (if ok "PASS" "FAIL") label (or detail ""))
  (unless ok (setq agr-failures (1+ agr-failures))))

(agr-check "package loads" (featurep 'agile-gtd))
(agr-check "agenda files resolve" (and org-agenda-files
                                       (seq-every-p #'file-exists-p org-agenda-files))
           (format "%d files" (length org-agenda-files)))


;;; Every range admits only what its own groups can hold.

;; This is the contract the reported bug broke: a `backlog' agenda grew [#F]
;; and [#G] headings because the filter read a missing cookie as the default
;; while the rank read the parent's cookie.  One number decides both now, so
;; an entry past the cutoff cannot be on screen at all.

(defun agr-ranked (query)
  "Return (HEADING . RANK) for every entry in the corpus matching QUERY."
  (org-ql-select (org-agenda-files) query
    :action (lambda () (cons (org-get-heading t t t t) (agile-gtd--item-rank)))))

(dolist (range agile-gtd-view-ranges)
  (let ((top (agile-gtd--rank-band-top (agile-gtd-view-range-priority range))))
    (dolist (spec (list (cons "next actions"
                              (agile-gtd-agenda-query-next-actions nil range t))
                        (cons "backlog"
                              (agile-gtd-agenda-query-backlog nil range))))
      (let* ((entries (agr-ranked (cdr spec)))
             (over (cl-remove-if (lambda (e) (<= (cdr e) top)) entries)))
        (agr-check (format "%s [%s] stays inside its cutoff band" (car spec) range)
                   (null over)
                   (format "%d entries, top=%d%s"
                           (length entries) top
                           (if over (format " — OVER: %S" (seq-take over 3)) "")))))))


;;; Widening only ever adds.

;; A range is a cutoff, so each one has to contain the one below it.  Were a
;; narrower range to hold something its wider neighbour drops, the cutoff
;; would not be a cutoff and rotation would lose work on the way out.

(let ((seen nil))
  (dolist (range agile-gtd-view-ranges)
    (let ((headings (mapcar #'car (agr-ranked
                                   (agile-gtd-agenda-query-next-actions nil range t)))))
      (when seen
        (let ((lost (cl-set-difference seen headings :test #'equal)))
          (agr-check (format "widening into %s drops nothing" range)
                     (null lost)
                     (format "%d entries%s" (length headings)
                             (if lost (format " — LOST: %S" (seq-take lost 3)) "")))))
      (setq seen headings))))


;;; Work scheduled for a later day is not backlog work.

(defun agr-future-scheduled-p ()
  "Return non-nil when the entry at point is scheduled after today."
  (when-let ((sc (org-element-property :scheduled (org-element-at-point))))
    (> (time-to-days (org-timestamp-to-time sc)) (time-to-days (current-time)))))

(dolist (range '(sprint backlog all))
  (dolist (spec (list (cons "next actions"
                            (agile-gtd-agenda-query-next-actions nil range t))
                      (cons "backlog"
                            (agile-gtd-agenda-query-backlog nil range))))
    (let ((deferred (org-ql-select (org-agenda-files) (cdr spec)
                      :action (lambda ()
                                (when (agr-future-scheduled-p)
                                  (org-get-heading t t t t))))))
      (setq deferred (delq nil deferred))
      (agr-check (format "%s [%s] holds nothing scheduled ahead" (car spec) range)
                 (null deferred)
                 (if deferred (format "%S" (seq-take deferred 3)) "")))))

(let ((deferred (org-ql-select (org-agenda-files)
                    (agile-gtd-agenda-query-backlog nil 'someday)
                  :action (lambda ()
                            (when (agr-future-scheduled-p)
                              (org-get-heading t t t t))))))
  (agr-check "backlog [someday] brings deferred work back"
             (delq nil deferred)
             (format "%d entries" (length (delq nil deferred)))))


;;; The rendered agenda

(defun agr-render (range)
  "Render the main agenda at RANGE and return its Next Actions section."
  (let ((agile-gtd--agenda-range-override range)
        (org-agenda-window-setup 'current-window))
    (unwind-protect
        (save-window-excursion
          (org-agenda nil "a")
          (with-current-buffer org-agenda-buffer-name
            (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
              (substring txt (string-match "Next Actions" txt)))))
      (when-let ((b (get-buffer org-agenda-buffer-name))) (kill-buffer b)))))

(defvar agr-backlog-section (agr-render 'backlog))
(defvar agr-someday-section (agr-render 'someday))

(let ((over (cl-loop for prio from (1+ agile-gtd-priority-default)
                     to agile-gtd-priority-lowest
                     when (string-match-p (format "\\[#%c\\] Priority %c" prio prio)
                                          agr-backlog-section)
                     collect prio)))
  (agr-check "rendered [backlog] has no heading past the default"
             (null over)
             (if over (format "found %S" (mapcar #'char-to-string over)) "")))

(let ((scheduled (string-match "^ *Scheduled$" agr-someday-section))
      (tickler (string-match "^ *Tickler$" agr-someday-section))
      (lowest (string-match (format "\\[#%c\\] Priority %c"
                                    agile-gtd-priority-lowest
                                    agile-gtd-priority-lowest)
                            agr-someday-section)))
  (agr-check "rendered [someday] has a Scheduled group" scheduled)
  (agr-check "Scheduled sits below every priority heading"
             (and scheduled lowest (< lowest scheduled)))
  (agr-check "Scheduled sits above Tickler"
             (and scheduled tickler (< scheduled tickler))))

;; Every non-parked entry the widest range admits with a future date belongs
;; under Scheduled.  Checking the headings rather than the count is what
;; catches one leaking back into a priority group.
(let* ((deferred (delq nil (org-ql-select (org-agenda-files)
                               (agile-gtd-agenda-query-next-actions nil 'someday t)
                             :action (lambda ()
                                       (when (and (agr-future-scheduled-p)
                                                  (not (member agile-gtd-someday-tag
                                                               (org-get-tags))))
                                         (org-get-heading t t t t))))))
       (start (string-match "^ *Scheduled$" agr-someday-section))
       (group (and start (substring agr-someday-section start
                                    (string-match "^ *Tickler$" agr-someday-section))))
       (missing (and group
                     (cl-remove-if (lambda (h) (string-match-p (regexp-quote h) group))
                                   deferred))))
  (agr-check "every deferred entry renders under Scheduled"
             (and deferred group (null missing))
             (format "%d deferred%s" (length deferred)
                     (if missing (format " — MISSING: %S" (seq-take missing 3)) ""))))

(message "\n%s: %d check(s) failed"
         (if (zerop agr-failures) "OK" "FAILURES") agr-failures)
(kill-emacs (if (zerop agr-failures) 0 1))

;;; agile-gtd-range-live.el ends here
