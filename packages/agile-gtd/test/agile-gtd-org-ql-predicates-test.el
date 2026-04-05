;;; agile-gtd-org-ql-predicates-test.el --- Tests for agile-gtd org-ql predicates -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-modern)
(require 'org-ql)
(require 'org-edna)
(require 'agile-gtd)

(defmacro agile-gtd-org-ql-test-with-sandbox (&rest body)
  "Run BODY with isolated Org and Agile GTD state."
  (declare (indent 0) (debug t))
  `(let* ((tmpdir (make-temp-file "agile-gtd-org-ql-test-" t))
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
          (org-tags-exclude-from-inheritance nil)
          (org-use-tag-inheritance t)
          (org-todo-keywords nil)
          (org-todo-repeat-to-state nil)
          (org-todo-keyword-faces nil)
          (org-priority-highest ?A)
          (org-priority-default ?B)
          (org-priority-lowest ?C)
          (org-priority-faces nil)
          (org-modern-priority nil)
          (agile-gtd-customers nil)
          (agile-gtd-project-files '("projects.org"))
          (agile-gtd-enable-agenda-files t)
          (agile-gtd-enable-refile-targets t)
          (agile-gtd-enable-org-modern-visuals t))
     (unwind-protect
         (progn
           ,@body)
       (ignore-errors (org-super-agenda-mode -1))
       (delete-directory tmpdir t))))

(defconst agile-gtd-org-ql-test-data
  "* PROJ Stuck project
** TODO Notes only

* PROJ Active project
** NEXT Active child

* PROJ Tickler project :SOMEDAY:
SCHEDULED: <2026-04-10 Fri>
** NEXT Inherited someday child
SCHEDULED: <2026-04-14 Tue>

* PROJ Tickler subtree
** NEXT Deferred child :SOMEDAY:
SCHEDULED: <2026-04-11 Sat>

* PROJ Mixed deferred subtree
** NEXT Deferred child with live sibling :SOMEDAY:
SCHEDULED: <2026-04-12 Sun>
** WAIT Live child

* NEXT Standalone next

* WAIT Standalone wait

* DONE Completed project
** NEXT Tangled next

* TODO Someday idea :SOMEDAY:

* TODO Work parent :#work:
** TODO Inherited work child
** TODO Primary work child :team:

* TODO Primary work root :#work:team:

* TODO Private errand :#personal:

* TODO Habit tag item :HABIT:

* TODO Habit style item
SCHEDULED: <2026-04-13 Mon .+1d>
:PROPERTIES:
:STYLE:    habit
:END:

* PROJ Sequential tasks
** NEXT Step one
   :PROPERTIES:
   :TRIGGER: next-sibling todo!(NEXT)
   :END:
** NEXT Step two
   :PROPERTIES:
   :BLOCKER: previous-sibling
   :END:

* PROJ Completed chain
** DONE Finished step
** NEXT Unblocked step
   :PROPERTIES:
   :BLOCKER: previous-sibling
   :END:
"
  "Org data used to exercise Agile GTD org-ql predicates.")

(defmacro agile-gtd-org-ql-test-with-data (&rest body)
  "Run BODY with predicate fixtures in a temporary Org buffer."
  (declare (indent 0) (debug t))
  `(agile-gtd-org-ql-test-with-sandbox
    (let* ((file (expand-file-name "predicate-fixtures.org" org-directory))
           (buffer nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert agile-gtd-org-ql-test-data))
            (setq buffer (find-file-noselect file))
            (with-current-buffer buffer
              (org-mode)
              (agile-gtd-enable)
              (org-set-regexps-and-options)
              ,@body))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun agile-gtd-org-ql-test-headings (buffer query)
  "Return headings in BUFFER matching QUERY."
  (org-ql-select buffer query :action '(org-get-heading t t t t)))

(defun agile-gtd-org-ql-test-assert-query (buffer query expected)
  "Assert that QUERY in BUFFER returns EXPECTED headings.

Runs the query once with preambles enabled and once with them disabled."
  (dolist (org-ql-use-preamble '(t nil))
    (ert-info ((format "query=%S preamble=%S" query org-ql-use-preamble))
      (should (equal (agile-gtd-org-ql-test-headings buffer query)
                     expected)))))

(ert-deftest agile-gtd-org-ql-predicates-match-tickler-queries ()
  (agile-gtd-org-ql-test-with-data
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-tickler)
    '("Tickler project"
      "Deferred child"
      "Deferred child with live sibling"))
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-tickler-proj)
    '("Tickler project"
      "Tickler subtree"))
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(and (heading "Inherited someday child")
          (agile-gtd-tickler))
    nil)))

(ert-deftest agile-gtd-org-ql-predicates-match-stuck-projects ()
  (agile-gtd-org-ql-test-with-data
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-stuck-proj)
    '("Stuck project"))
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-stuck)
    '("Stuck project"))))

(ert-deftest agile-gtd-org-ql-predicates-separate-standalone-and-tangled-actions ()
  (agile-gtd-org-ql-test-with-data
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-standalone-next)
    '("Standalone next"
      "Standalone wait"))
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-tangling)
    '("Tangled next"))))

(ert-deftest agile-gtd-org-ql-predicates-classify-work-context ()
  (agile-gtd-org-ql-test-with-data
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(and (todo)
          (agile-gtd-work))
    '("Work parent"
      "Inherited work child"
      "Primary work child"
      "Primary work root"))
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(and (heading "Private errand")
          (agile-gtd-private))
    '("Private errand"))
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(and (heading "Work parent")
          (agile-gtd-private))
    nil)))

(ert-deftest agile-gtd-org-ql-predicates-match-someday-and-habits ()
  (agile-gtd-org-ql-test-with-data
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-someday)
    '("Inherited someday child"
      "Someday idea"))
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(and (heading "Tickler project")
          (agile-gtd-someday))
    nil)
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(and (heading "Deferred child")
          (agile-gtd-someday))
    nil)
   (agile-gtd-org-ql-test-assert-query
    buffer
    '(agile-gtd-habit)
    '("Habit tag item"
      "Habit style item"))))

(ert-deftest agile-gtd-org-ql-predicates-blocked ()
  "agile-gtd-blocked matches entries with unsatisfied BLOCKER, not satisfied ones."
  (agile-gtd-org-ql-test-with-data
   (let ((org-blocker-hook (list #'org-edna-blocker-function)))
     ;; Step two is blocked: previous sibling (Step one) is NEXT, not done
     (agile-gtd-org-ql-test-assert-query
      buffer
      '(agile-gtd-blocked)
      '("Step two"))
     ;; Unblocked step: previous sibling (Finished step) is DONE
     (agile-gtd-org-ql-test-assert-query
      buffer
      '(and (heading "Unblocked step") (agile-gtd-blocked))
      nil)
     ;; Item with no BLOCKER property is never blocked
     (agile-gtd-org-ql-test-assert-query
      buffer
      '(and (heading "Standalone next") (agile-gtd-blocked))
      nil))))

(provide 'agile-gtd-org-ql-predicates-test)

;;; agile-gtd-org-ql-predicates-test.el ends here
