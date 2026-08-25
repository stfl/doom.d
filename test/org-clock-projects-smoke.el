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

(opc-check "resolver wired to agile-gtd"
           (eq org-clock-projects-files-function #'agile-gtd-project-files))
(let ((files (funcall org-clock-projects-files-function)))
  (opc-check "resolver returns the real project list"
             (and (= (length files) (length agile-gtd-projects))
                  (member "oebb.org" files))
             (format "%d files" (length files))))

;; Relative names from the registry must resolve against org-directory.
(let* ((files (funcall org-clock-projects-files-function))
       (expanded (mapcar (lambda (f) (file-truename (expand-file-name f org-directory)))
                         files)))
  (opc-check "project files exist on disk"
             (seq-every-p #'file-exists-p expanded)
             (format "%d/%d" (seq-count #'file-exists-p expanded) (length expanded))))

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

;; The advice the package absorbed must be gone from the config.
(dolist (sym '(stfl/org-clock-continue? stfl/org-clock-continous-threshold
               stfl/org-time-minutes-ago stfl/org-time-minutes-ago-rounded
               stfl/org-time-format-ago))
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
                  ("SPC n o"   org-clock-projects-switch)))
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
