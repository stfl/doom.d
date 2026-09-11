;;; agile-gtd-keys.el --- Key lookups for the agenda priority and range chords -*- lexical-binding: t; -*-

;; A binding is silent about whether the command behind it exists, and a
;; command is silent about whether anything reaches it, so this asserts both
;; ends: that `agile-gtd' defines the range commands this configuration binds,
;; and that the keys resolve to them.  Run it with:
;;
;;   emacs -q --batch -l ~/.config/doom/test/bootstrap.el \\
;;         -l ~/.config/doom/test/agile-gtd-keys.el

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'agile-gtd)

(defvar agk-failures 0)
(defun agk-check (label ok &optional detail)
  (message "%s %-52s %s" (if ok "PASS" "FAIL") label (or detail ""))
  (unless ok (setq agk-failures (1+ agk-failures))))

(agk-check "package loads" (featurep 'agile-gtd))

;; The four named ranges are the vocabulary every binding below selects from.
(agk-check "range vocabulary is narrowest-first"
           (equal (bound-and-true-p agile-gtd-view-ranges)
                  '(sprint backlog all someday))
           (format "%S" (bound-and-true-p agile-gtd-view-ranges)))

(dolist (cmd '(agile-gtd-agenda-set-range
               agile-gtd-agenda-wider-range
               agile-gtd-agenda-narrower-range
               agile-gtd-agenda-reset-show-priorities))
  (agk-check (format "%s is a command" cmd) (commandp cmd)))

;; The two TODO chords are this configuration's own, so nothing else defines
;; them and a typo in either name costs a key rather than reporting itself.
(dolist (sym '(stfl/org-agenda-todo-forward stfl/org-agenda-todo-backward))
  (agk-check (format "%s is defined here" sym) (commandp sym)))

;; Chords live directly in `org-agenda-mode-map', which `lookup-key' reaches
;; without an agenda buffer or an evil state to put the map on.
(dolist (spec '(("C-S-k" org-agenda-priority-up)
                ("C-S-j" org-agenda-priority-down)
                ("C-S-h" stfl/org-agenda-todo-backward)
                ("C-S-l" stfl/org-agenda-todo-forward)
                ("C-M-k" agile-gtd-agenda-narrower-range)
                ("C-M-j" agile-gtd-agenda-wider-range)))
  (let ((bound (lookup-key org-agenda-mode-map (kbd (car spec)))))
    (agk-check (format "%s -> %s" (car spec) (cadr spec))
               (eq bound (cadr spec))
               (format "got %s" bound))))

;; One prefix covers everything done to an item's priority and to the view's,
;; so the range keys answer under `p' rather than a prefix of their own.
(with-temp-buffer
  (org-agenda-mode)
  (dolist (spec '(("SPC m p p" org-agenda-priority)
                  ("SPC m p s" agile-gtd-agenda-toggle-someday)
                  ("SPC m p t" agile-gtd-agenda-toggle-tickler)
                  ("SPC m p v" agile-gtd-agenda-set-range)
                  ("SPC m p n" agile-gtd-agenda-narrower-range)
                  ("SPC m p w" agile-gtd-agenda-wider-range)))
    (let ((bound (key-binding (kbd (car spec)))))
      (agk-check (format "%s -> %s" (car spec) (cadr spec))
                 (eq bound (cadr spec))
                 (format "got %s" bound))))
  ;; The view prefix folded into the priority prefix, so nothing answers there.
  (agk-check "SPC m v is free"
             (not (commandp (key-binding (kbd "SPC m v v"))))
             (format "got %s" (key-binding (kbd "SPC m v v")))))

(message "\n=== %d failure(s) ===" agk-failures)
;; Exit non-zero so a failure is visible to something other than a human
;; reading the output.
(kill-emacs (min agk-failures 1))

(provide 'agile-gtd-keys)
;;; agile-gtd-keys.el ends here
