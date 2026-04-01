;;; agile-gtd-test.el --- Tests for agile-gtd -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-modern)
(require 'agile-gtd)

(defmacro agile-gtd-test-with-sandbox (&rest body)
  "Run BODY with isolated Org and Agile GTD state."
  (declare (indent 0) (debug t))
  `(let* ((tmpdir (make-temp-file "agile-gtd-test-" t))
          (org-directory tmpdir)
          (org-agenda-files nil)
          (org-agenda-diary-file nil)
          (org-agenda-custom-commands nil)
          (org-capture-templates nil)
          (org-refile-targets nil)
          (org-refile-use-outline-path nil)
          (org-outline-path-complete-in-steps nil)
          (org-refile-allow-creating-parent-nodes nil)
          (org-stuck-projects nil)
          (org-super-agenda-header-separator nil)
          (org-tag-alist '(("@home" . ?h)))
          (org-todo-keywords nil)
          (org-todo-repeat-to-state nil)
          (org-todo-keyword-faces nil)
          (org-priority-highest ?A)
          (org-priority-default ?B)
          (org-priority-lowest ?C)
          (org-priority-faces nil)
          (org-modern-priority nil)
          (agile-gtd-primary-work-tags '("team"))
          (agile-gtd-project-files '("projects.org"))
          (agile-gtd-enable-agenda-files t)
          (agile-gtd-enable-refile-targets t)
          (agile-gtd-enable-org-modern-visuals t))
     (unwind-protect
         (progn
           ,@body)
       (ignore-errors (org-super-agenda-mode -1))
       (delete-directory tmpdir t))))

(ert-deftest agile-gtd-enable-applies-core-settings ()
  (agile-gtd-test-with-sandbox
    (agile-gtd-enable)
    (should org-super-agenda-mode)
    (should (equal org-agenda-files
                   (mapcar (lambda (file)
                             (expand-file-name file org-directory))
                           '("inbox.org"
                             "inbox-orgzly.org"
                             "todo.org"
                             "projects.org"))))
    (should (equal org-agenda-diary-file
                   (expand-file-name "diary.org" org-directory)))
    (should (= org-priority-highest agile-gtd-priority-highest))
    (should (= org-priority-default agile-gtd-priority-default))
    (should (= org-priority-lowest agile-gtd-priority-lowest))
    (should (equal org-priority-faces (agile-gtd--priority-faces)))
    (should (equal org-modern-priority (agile-gtd--priority-symbols)))
    (should (equal org-todo-keywords agile-gtd-todo-keywords))
    (should (equal org-todo-repeat-to-state agile-gtd-todo-repeat-to-state))
    (should (equal org-refile-targets
                   '((nil :maxlevel . 9)
                     (org-agenda-files :maxlevel . 4)
                     (agile-gtd--someday-files :maxlevel . 4))))
    (should (equal org-stuck-projects (agile-gtd--stuck-projects-setting)))
    (should (assoc "P" org-capture-templates))
    (should (assoc "a" org-agenda-custom-commands))))

(ert-deftest agile-gtd-refresh-does-not-duplicate-workflow-tags ()
  (agile-gtd-test-with-sandbox
    (let ((expected nil))
      (setq org-tag-alist '(("@home" . ?h)
                            ("custom" . ?c)))
      (setq expected (append (copy-tree org-tag-alist)
                             (agile-gtd--workflow-tag-alist)))
      (agile-gtd-refresh)
      (should (equal org-tag-alist expected))
      (agile-gtd-refresh)
      (should (equal org-tag-alist expected)))))

(ert-deftest agile-gtd-refresh-handles-empty-primary-work-tags ()
  (agile-gtd-test-with-sandbox
    (let ((agile-gtd-primary-work-tags nil))
      (agile-gtd-refresh)
      (should (equal (agile-gtd--primary-work-query)
                     '(and (tags "__agile-gtd-no-primary-work__")
                           (not (tags "__agile-gtd-no-primary-work__")))))
      (should (equal (agile-gtd--today-groups-no-primary-work)
                     (agile-gtd--today-groups))))))

(ert-deftest agile-gtd-capture-templates-include-priority-and-protocol-support ()
  (agile-gtd-test-with-sandbox
    (let* ((templates (agile-gtd--capture-templates))
           (project (assoc "p" templates))
           (protocol (assoc "P" templates)))
      (should project)
      (should protocol)
      (should (string-match-p (regexp-quote (agile-gtd--priority-prompt-choices))
                              (nth 4 project)))
      (should (string-match-p (regexp-quote "agile-gtd--protocol-description")
                              (nth 4 protocol))))))

(ert-deftest agile-gtd-ancestor-priority-groups-cover-the-range ()
  (agile-gtd-test-with-sandbox
    (let ((groups (agile-gtd-ancestor-priority-groups)))
      (should (= (length groups)
                 (+ 3 (length (agile-gtd--priority-range)))))
      (should (equal (plist-get (nth 0 groups) :name) "Tickler"))
      (should (equal (plist-get (nth 1 groups) :name) "Someday"))
      (should (equal (plist-get (car (last groups)) :name)
                     "Default Priority (Rest)")))))

(ert-deftest agile-gtd-query-helpers-return-stable-sexps ()
  (agile-gtd-test-with-sandbox
    (should (equal (agile-gtd-not-someday-habit)
                   '(not (or (tags "SOMEDAY" "HABIT")
                             (habit)))))
    (should (equal (agile-gtd-not-sched-or-dead 14)
                   '(and (not (scheduled :from today))
                         (not (deadline :from 14)))))
    (should (equal (agile-gtd-agenda-query-stuck-projects)
                   '(agile-gtd-stuck-proj)))
    (should (equal (agile-gtd-action-keywords)
                   '("NEXT" "WAIT")))
    (should (equal (agile-gtd-project-keyword) "PROJ"))))

(ert-deftest agile-gtd-protocol-description-normalizes-brackets ()
  (should (equal (agile-gtd--protocol-description "[hello] [world]")
                 "(hello) (world)")))

;;; agile-gtd-test.el ends here
