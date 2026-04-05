;;; agile-gtd-agenda-test.el --- Tests for agile-gtd agenda query functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-modern)
(require 'org-ql)
(require 'org-edna)
(require 'agile-gtd)

;; Reuse the sandbox and helpers from predicates tests
(require 'agile-gtd-org-ql-predicates-test)

(defconst agile-gtd-agenda-test-data
  "* NEXT [#A] Work high-prio :#work:

* NEXT [#B] Private medium :#personal:

* NEXT Work with deadline :#work:
DEADLINE: <2026-04-06 Mon>

* NEXT No priority private

* PROJ [#C] Stuck work :#work:
** TODO Notes only

* PROJ Stuck private
** TODO Notes only

* TODO Inbox item one :#inbox:

* TODO Inbox item two :inbox:

* DONE Finished inbox :#inbox:

* NEXT [#A] Blocking action

* NEXT [#A] Blocked action
:PROPERTIES:
:BLOCKER:  previous-sibling
:END:
"
  "Org data for agenda query tests.")

(defmacro agile-gtd-agenda-test-with-data (&rest body)
  "Run BODY with agenda query fixtures in a temporary Org buffer."
  (declare (indent 0) (debug t))
  `(agile-gtd-org-ql-test-with-sandbox
    (let* ((file (expand-file-name "agenda-fixtures.org" org-directory))
           (buffer nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert agile-gtd-agenda-test-data))
            (setq buffer (find-file-noselect file))
            (with-current-buffer buffer
              (org-mode)
              (agile-gtd-enable)
              (org-set-regexps-and-options)
              ,@body))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

;;; Structure tests — verify query functions return well-formed sexps

(ert-deftest agile-gtd-agenda-query-next-actions-returns-sexp ()
  "Next actions query returns a well-formed sexp with and without filter."
  (let ((query (agile-gtd-agenda-query-next-actions)))
    (should (listp query))
    (should (eq 'and (car query))))
  (let ((filtered (agile-gtd-agenda-query-next-actions '(agile-gtd-work))))
    (should (listp filtered))
    ;; Filtered query wraps with an outer `and'
    (should (eq 'and (car filtered)))))

(ert-deftest agile-gtd-query-next-actions-uses-sprint-prio-threshold ()
  "Next-actions query uses `agile-gtd-sprint-prio-threshold' as priority cut-off."
  ;; Default threshold C
  (let* ((agile-gtd-max-priority-group nil)
         (agile-gtd-sprint-prio-threshold ?C)
         (org-priority-default ?E)
         (query     (agile-gtd-agenda-query-next-actions))
         (inner-and (nth 2 query))
         (or-clause (cadr inner-and)))
    (should (= (agile-gtd--current-max-priority-group) ?C))
    (should (member `(priority >= (char-to-string ,?C)) or-clause))
    (should (member `(agile-gtd-deadline-prio <= ,?C) or-clause)))
  ;; Alternate threshold B — confirms threshold is not hard-coded
  (let* ((agile-gtd-max-priority-group nil)
         (agile-gtd-sprint-prio-threshold ?B)
         (org-priority-default ?E)
         (query     (agile-gtd-agenda-query-next-actions))
         (inner-and (nth 2 query))
         (or-clause (cadr inner-and)))
    (should (= (agile-gtd--current-max-priority-group) ?B))
    (should (member `(priority >= (char-to-string ,?B)) or-clause))
    (should (member `(agile-gtd-deadline-prio <= ,?B) or-clause))))

(ert-deftest agile-gtd-agenda-query-backlog-returns-sexp ()
  "Backlog query returns a well-formed sexp with and without filter."
  (let ((query (agile-gtd-agenda-query-backlog)))
    (should (listp query))
    (should (eq 'and (car query))))
  (let ((filtered (agile-gtd-agenda-query-backlog '(agile-gtd-work))))
    (should (listp filtered))
    (should (eq 'and (car filtered)))))

(ert-deftest agile-gtd-agenda-query-stuck-projects-with-and-without-filter ()
  "Stuck projects query supports optional tag-filter."
  (should (equal (agile-gtd-agenda-query-stuck-projects)
                 '(agile-gtd-stuck-proj)))
  (should (equal (agile-gtd-agenda-query-stuck-projects '(agile-gtd-work))
                 '(and (agile-gtd-stuck-proj) (agile-gtd-work))))
  (should (equal (agile-gtd-agenda-query-stuck-projects '(agile-gtd-private))
                 '(and (agile-gtd-stuck-proj) (agile-gtd-private)))))

;;; Area-filter tests — use simpler org-ql queries that work in batch

(ert-deftest agile-gtd-agenda-query-stuck-projects-area-filter ()
  "Work filter: only stuck work projects; private filter: only stuck private."
  (agile-gtd-agenda-test-with-data
   (let* ((work-query (agile-gtd-agenda-query-stuck-projects '(agile-gtd-work)))
          (work-headings (agile-gtd-org-ql-test-headings buffer work-query))
          (private-query (agile-gtd-agenda-query-stuck-projects '(agile-gtd-private)))
          (private-headings (agile-gtd-org-ql-test-headings buffer private-query)))
     (should (member "Stuck work" work-headings))
     (should-not (member "Stuck private" work-headings))
     (should (member "Stuck private" private-headings))
     (should-not (member "Stuck work" private-headings)))))

(ert-deftest agile-gtd-agenda-query-backlog-area-filter ()
  "Backlog query respects area filter."
  (agile-gtd-agenda-test-with-data
   (let* ((work-query (agile-gtd-agenda-query-backlog '(agile-gtd-work)))
          (work-headings (agile-gtd-org-ql-test-headings buffer work-query))
          (private-query (agile-gtd-agenda-query-backlog '(agile-gtd-private)))
          (private-headings (agile-gtd-org-ql-test-headings buffer private-query)))
     ;; Work backlog should include work projects
     (should (member "Stuck work" work-headings))
     (should-not (member "Stuck private" work-headings))
     ;; Private backlog should include private projects and standalone actions
     (should (member "Stuck private" private-headings))
     (should-not (member "Stuck work" private-headings)))))

(ert-deftest agile-gtd-agenda-query-next-actions-excludes-blocked ()
  "Next-actions query excludes entries whose BLOCKER is unsatisfied."
  (agile-gtd-agenda-test-with-data
   (let ((org-blocker-hook (list #'org-edna-blocker-function)))
     (let* ((query    (agile-gtd-agenda-query-next-actions))
            (headings (agile-gtd-org-ql-test-headings buffer query)))
       ;; Blocked action has BLOCKER: previous-sibling (Blocking action is NEXT)
       (should-not (member "Blocked action" headings))
       ;; The blocker itself has no BLOCKER property and must still appear
       (should (member "Blocking action" headings))))))

(ert-deftest agile-gtd-agenda-query-backlog-excludes-blocked ()
  "Backlog query excludes entries whose BLOCKER is unsatisfied."
  (agile-gtd-agenda-test-with-data
   (let ((org-blocker-hook (list #'org-edna-blocker-function)))
     (let* ((query    (agile-gtd-agenda-query-backlog))
            (headings (agile-gtd-org-ql-test-headings buffer query)))
       (should-not (member "Blocked action" headings))
       (should (member "Blocking action" headings))))))

(ert-deftest agile-gtd-agenda-actions-work-private-split ()
  "Action items split correctly between work and private via direct predicates."
  (agile-gtd-agenda-test-with-data
   (let* ((work-headings (agile-gtd-org-ql-test-headings
                          buffer
                          '(and (todo "NEXT" "WAIT") (agile-gtd-work))))
          (private-headings (agile-gtd-org-ql-test-headings
                             buffer
                             '(and (todo "NEXT" "WAIT") (agile-gtd-private)))))
     ;; Work actions
     (should (member "Work high-prio" work-headings))
     (should (member "Work with deadline" work-headings))
     (should-not (member "Private medium" work-headings))
     (should-not (member "No priority private" work-headings))
     ;; Private actions
     (should (member "Private medium" private-headings))
     (should (member "No priority private" private-headings))
     (should-not (member "Work high-prio" private-headings)))))

;;; Rank ordering test — uses agile-gtd--item-rank directly

(ert-deftest agile-gtd-agenda-rank-ordering ()
  "Items sorted by `agile-gtd--item-rank' come out in correct priority order."
  (agile-gtd-agenda-test-with-data
   (let* ((ranked (org-ql-select buffer '(todo "NEXT" "WAIT")
                    :action (lambda ()
                              (cons (org-get-heading t t t t)
                                    (agile-gtd--item-rank))))))
     ;; [#A] rank 1 should come before [#B] rank 11
     (should (< (cdr (assoc "Work high-prio" ranked))
                (cdr (assoc "Private medium" ranked))))
     ;; Deadline item should get a rank from deadline (3 days out = rank 10)
     (should (< (cdr (assoc "Work with deadline" ranked))
                (cdr (assoc "No priority private" ranked))))
     ;; No-priority default rank (agile-gtd--rank-default) should be highest
     (should (= (agile-gtd--rank-default) (cdr (assoc "No priority private" ranked)))))))

;;; Inbox query tests

(ert-deftest agile-gtd-agenda-query-inbox-returns-sexp ()
  "Inbox query returns a well-formed sexp."
  (let ((query (agile-gtd-agenda-query-inbox)))
    (should (listp query))
    (should (eq 'and (car query)))))

(ert-deftest agile-gtd-agenda-query-inbox-matches ()
  "Inbox query matches undone inbox-tagged items, excludes done ones."
  (agile-gtd-agenda-test-with-data
   (let* ((query (agile-gtd-agenda-query-inbox))
          (headings (agile-gtd-org-ql-test-headings buffer query)))
     (should (member "Inbox item one" headings))
     (should (member "Inbox item two" headings))
     (should-not (member "Finished inbox" headings))
     (should-not (member "Work high-prio" headings)))))

;;; Customer agenda command tests

(ert-deftest agile-gtd-customer-agenda-commands-generated ()
  "Customer agenda commands are generated for each configured customer."
  (agile-gtd-agenda-test-with-data
   (let ((agile-gtd-customers '((:tag "acme" :name "ACME Corp" :key ?a)
                                 (:tag "globex" :name "Globex" :key ?g))))
     (let ((cmds (agile-gtd--customer-agenda-commands)))
       (should (= (length cmds) 2))
       (should (equal (caar cmds) "wa"))
       (should (equal (caadr cmds) "wg"))
       (should (string-match-p "ACME Corp" (cadar cmds)))
       (should (string-match-p "Globex" (car (cdar (cdr cmds)))))))))

(ert-deftest agile-gtd-customer-tags-added-to-tag-alist ()
  "Customer tags appear in org-tag-alist after refresh."
  (agile-gtd-agenda-test-with-data
   (let ((agile-gtd-customers '((:tag "acme" :name "ACME" :key ?a)
                                 (:tag "globex" :name "Globex" :key ?g))))
     (agile-gtd-refresh)
     (should (assoc "acme" org-tag-alist))
     (should (equal (cdr (assoc "acme" org-tag-alist)) ?a))
     (should (assoc "globex" org-tag-alist))
     (should (equal (cdr (assoc "globex" org-tag-alist)) ?g)))))

(ert-deftest agile-gtd-customer-tags-not-duplicated ()
  "Customer tags are not duplicated across multiple refreshes."
  (agile-gtd-agenda-test-with-data
   (let ((agile-gtd-customers '((:tag "acme" :name "ACME" :key ?a))))
     (agile-gtd-refresh)
     (agile-gtd-refresh)
     (should (= 1 (cl-count "acme" org-tag-alist :key #'car-safe :test #'equal))))))

;;; Agenda custom command structure tests

(defun agile-gtd-test--collect-settings-values (tree key)
  "Walk agenda command TREE and collect all values bound to settings KEY."
  (let (results)
    (cl-labels ((walk (node)
                  (when (proper-list-p node)
                    (if (eq (car node) key)
                        (when (cdr node)
                          (push (cadr node) results))
                      (mapc #'walk node)))))
      (walk tree))
    results))

(ert-deftest agile-gtd-agenda-custom-commands-is-list ()
  "agile-gtd--agenda-custom-commands returns a non-empty list."
  (agile-gtd-agenda-test-with-data
   (let ((cmds (agile-gtd--agenda-custom-commands)))
     (should (listp cmds))
     (should (> (length cmds) 0)))))

(ert-deftest agile-gtd-agenda-super-groups-are-quoted ()
  "Every org-super-agenda-groups value in the agenda commands is a quoted
form so that org-agenda can eval it without triggering 'Invalid function'."
  (agile-gtd-agenda-test-with-data
   (let* ((cmds (agile-gtd--agenda-custom-commands))
          (vals (agile-gtd-test--collect-settings-values
                 cmds 'org-super-agenda-groups)))
     (should (> (length vals) 0))
     (dolist (val vals)
       ;; Each value must be either (quote ...) or a plain symbol —
       ;; never a bare list that would error when eval'd by org-agenda.
       (should (or (symbolp val)
                   (and (consp val) (eq 'quote (car val)))))))))

(ert-deftest agile-gtd-agenda-super-groups-eval-to-lists ()
  "Every org-super-agenda-groups value evaluates to a proper list."
  (agile-gtd-agenda-test-with-data
   (let* ((cmds (agile-gtd--agenda-custom-commands))
          (vals (agile-gtd-test--collect-settings-values
                 cmds 'org-super-agenda-groups)))
     (should (> (length vals) 0))
     (dolist (val vals)
       (should (listp (eval val t)))))))

(defun agile-gtd-test--collect-ql-block-queries (tree)
  "Walk agenda command TREE collecting org-ql-block query arguments."
  ;; Each org-ql-block form is (org-ql-block QUERY SETTINGS).
  ;; QUERY is (cadr form).
  (let (results)
    (cl-labels ((walk (node)
                  (when (proper-list-p node)
                    (if (eq (car node) 'org-ql-block)
                        (when (cdr node)
                          (push (cadr node) results))
                      (mapc #'walk node)))))
      (walk tree))
    results))

(ert-deftest agile-gtd-agenda-ql-block-queries-are-quoted ()
  "Every org-ql-block query in the agenda commands is a quoted or
nested-backquote form — never a bare evaluated list that would call
org-ql predicates outside of any heading context."
  (agile-gtd-agenda-test-with-data
   (let* ((cmds (agile-gtd--agenda-custom-commands))
          (queries (agile-gtd-test--collect-ql-block-queries cmds)))
     (should (> (length queries) 0))
     (dolist (q queries)
       ;; Acceptable forms: (quote ...), (backquote ...), symbol, or a
       ;; bare function-call form like (agile-gtd-agenda-query-stuck-projects)
       ;; whose car is a known function (call-at-eval-time pattern).
       (should (or (symbolp q)
                   ;; (quote ...) — quoted at define-time
                   (and (consp q) (eq 'quote (car q)))
                   ;; nested backquote — expanded at agenda-build-time
                   (and (consp q) (eq '\` (car q)))
                   ;; bare function call like (func) — eval calls func
                   (and (consp q) (symbolp (car q)) (fboundp (car q)))))))))

(ert-deftest agile-gtd-agenda-ql-block-queries-eval-to-sexps ()
  "Every org-ql-block query evaluates to a proper list (org-ql sexp)
without signalling an error."
  (agile-gtd-agenda-test-with-data
   (let* ((cmds (agile-gtd--agenda-custom-commands))
          (queries (agile-gtd-test--collect-ql-block-queries cmds)))
     (should (> (length queries) 0))
     (dolist (q queries)
       (should (listp (eval q t)))))))

(ert-deftest agile-gtd-agenda-today-groups-has-time-grid ()
  "The today agenda groups include a :time-grid entry."
  (let* ((groups (agile-gtd--today-groups))
         (has-time-grid (cl-some (lambda (g) (plist-get g :time-grid)) groups)))
    (should has-time-grid)))

(ert-deftest agile-gtd-agenda-day-block-super-groups-evaluable ()
  "The agenda day block's org-super-agenda-groups value evaluates to a list."
  (let* ((block (agile-gtd--agenda-day))
         (settings (nth 2 block))
         (groups-pair (assq 'org-super-agenda-groups settings)))
    (should groups-pair)
    (should (listp (eval (cadr groups-pair) t)))))

;;; Agenda buffer integration tests

(defun agile-gtd-agenda-integration-test-org-data ()
  "Return org data for agenda integration tests.
Uses today's date so the scheduled items appear in the day block."
  (concat "* NEXT [#A] High priority action\n\n"
          "* NEXT [#C] Medium priority action\n\n"
          "* PROJ Stuck project\n"
          "** TODO Notes only\n\n"
          ;; Scheduled today: appears in day block but NOT in Next Actions
          ;; (excluded by `not (scheduled)').  No work tag → private only.
          "* NEXT [#B] Scheduled today\n"
          (format "SCHEDULED: <%s>\n" (format-time-string "%Y-%m-%d %a"))
          ;; Work item scheduled today: appears in day block in work views only.
          "* NEXT [#A] Work item today :#work:\n"
          (format "SCHEDULED: <%s>\n" (format-time-string "%Y-%m-%d %a"))))

(defmacro agile-gtd-agenda-test-build-view (cmd-key &rest body)
  "Build org agenda for CMD-KEY in a fresh sandbox.
BODY executes with `agenda-text' bound to the resulting agenda buffer's text."
  (declare (indent 1) (debug t))
  `(agile-gtd-org-ql-test-with-sandbox
    (let* ((agile-gtd-project-files '("agenda-integration.org"))
           (file (expand-file-name "agenda-integration.org" org-directory))
           (org-agenda-window-setup 'current-window))
      (with-temp-file file
        (insert (agile-gtd-agenda-integration-test-org-data)))
      (agile-gtd-enable)
      ;; agile-gtd-enable adds inbox/todo/etc. files that don't exist in
      ;; the sandbox.  Override org-agenda-files to only our test fixture
      ;; so org-agenda doesn't prompt about missing files.
      (setq org-agenda-files (list file))
      (unwind-protect
          (save-window-excursion
            (org-agenda nil ,cmd-key)
            (let* ((buf (get-buffer org-agenda-buffer-name))
                   (agenda-text (with-current-buffer buf (buffer-string))))
              ,@body))
        (when-let ((buf (get-buffer org-agenda-buffer-name)))
          (kill-buffer buf))))))

(ert-deftest agile-gtd-agenda-main-view-all-sections-populated ()
  "Main Agenda ('a') builds with all three blocks populated by test items."
  (agile-gtd-agenda-test-build-view "a"
    ;; Day agenda block: today-scheduled item appears
    (ert-info ("Day block")
      (should (string-match-p "Scheduled today" agenda-text)))
    ;; Stuck Projects block: header present and stuck item present
    (ert-info ("Stuck Projects block")
      (should (string-match-p "Stuck Projects" agenda-text))
      (should (string-match-p "Stuck project" agenda-text)))
    ;; Next Actions block: header present and unscheduled priority items present
    (ert-info ("Next Actions block")
      (should (string-match-p "Next Actions" agenda-text))
      (should (string-match-p "High priority action" agenda-text))
      (should (string-match-p "Medium priority action" agenda-text)))))

(ert-deftest agile-gtd-agenda-main-view-section-order ()
  "In Main Agenda ('a'): Stuck Projects section precedes Next Actions."
  (agile-gtd-agenda-test-build-view "a"
    (let ((stuck-pos (string-match "Stuck Projects" agenda-text))
          (next-pos  (string-match "Next Actions"   agenda-text)))
      (should stuck-pos)
      (should next-pos)
      (should (< stuck-pos next-pos)))))

(ert-deftest agile-gtd-agenda-main-view-scheduled-excluded-from-next-actions ()
  "Scheduled items do not appear under the Next Actions section."
  (agile-gtd-agenda-test-build-view "a"
    (let ((next-pos (string-match "Next Actions" agenda-text)))
      (should next-pos)
      ;; The substring from the Next Actions header onward must not
      ;; contain the scheduled-today item.
      (should-not
       (string-match-p "Scheduled today"
                        (substring agenda-text next-pos))))))

(ert-deftest agile-gtd-agenda-stuck-projects-view-populated ()
  "Stuck Projects view ('rs') shows stuck items."
  (agile-gtd-agenda-test-build-view "rs"
    (should (string-match-p "Stuck Projects" agenda-text))
    (should (string-match-p "Stuck project" agenda-text))))

(ert-deftest agile-gtd-agenda-private-view-sections-populated ()
  "Private Agenda ('pp') contains private items in all its blocks."
  (agile-gtd-agenda-test-build-view "pp"
    (should (string-match-p "Next Actions" agenda-text))
    (should (string-match-p "High priority action" agenda-text))
    (should (string-match-p "Stuck Projects" agenda-text))
    (should (string-match-p "Stuck project" agenda-text))))

;;; Today-block tag-filter tests

(ert-deftest agile-gtd-agenda-day-block-tag-filter-uses-skip-function ()
  "Agenda day block with tag filter uses org-agenda-skip-function,
not the broken org-agenda-tag-filter-preset (which has no effect in
compound commands because org-agenda-prepare runs before lprops bind)."
  (let* ((block (agile-gtd--agenda-day '("+#work")))
         (settings (nth 2 block))
         (skip-pair (assq 'org-agenda-skip-function settings))
         (preset-pair (assq 'org-agenda-tag-filter-preset settings)))
    (should skip-pair)
    (should-not preset-pair)
    ;; Value must be a quoted form: (quote (when ...))
    (should (and (consp (cadr skip-pair))
                 (eq 'quote (car (cadr skip-pair)))))))

(ert-deftest agile-gtd-agenda-private-today-excludes-work-items ()
  "Private Agenda ('pp') today block excludes work-tagged items."
  (agile-gtd-agenda-test-build-view "pp"
    ;; Non-work item scheduled today must appear
    (should (string-match-p "Scheduled today" agenda-text))
    ;; Work-tagged item must not appear anywhere in the private view
    (should-not (string-match-p "Work item today" agenda-text))))

(ert-deftest agile-gtd-agenda-work-today-includes-only-work-items ()
  "Work Agenda ('ww') today block shows work items, excludes non-work ones."
  (agile-gtd-agenda-test-build-view "ww"
    ;; Work item scheduled today must appear
    (should (string-match-p "Work item today" agenda-text))
    ;; Non-work scheduled item must not appear in the work view
    (should-not (string-match-p "Scheduled today" agenda-text))))

;;; hide-today parameter tests

(defun agile-gtd-agenda-test-hide-today-data ()
  "Return org data for hide-today parameter tests.
Uses relative dates so scheduled/deadline items are testable."
  (let ((today (format-time-string "%Y-%m-%d %a"))
        (yesterday (format-time-string "%Y-%m-%d %a"
                     (time-subtract (current-time) (days-to-time 1))))
        (tomorrow (format-time-string "%Y-%m-%d %a"
                    (time-add (current-time) (days-to-time 1)))))
    (concat
     "* NEXT [#A] Plain next action\n\n"
     "* NEXT [#A] Scheduled today\n"
     (format "SCHEDULED: <%s>\n\n" today)
     "* NEXT [#A] Deadline today\n"
     (format "DEADLINE: <%s>\n\n" today)
     "* NEXT [#A] Scheduled yesterday (overdue)\n"
     (format "SCHEDULED: <%s>\n\n" yesterday)
     "* NEXT [#A] Deadline yesterday (overdue)\n"
     (format "DEADLINE: <%s>\n\n" yesterday)
     "* NEXT [#A] Scheduled tomorrow (future)\n"
     (format "SCHEDULED: <%s>\n\n" tomorrow)
     "* NEXT [#A] Someday item :SOMEDAY:\n\n"
     "* NEXT [#A] Tickler today :SOMEDAY:\n"
     (format "SCHEDULED: <%s>\n\n" today)
     "* NEXT [#A] Tickler future :SOMEDAY:\n"
     (format "SCHEDULED: <%s>\n\n" tomorrow)
     "* NEXT [#A] Habit item :HABIT:\n\n")))

(defmacro agile-gtd-agenda-test-hide-today-data-do (&rest body)
  "Run BODY with hide-today test fixtures in a temporary Org buffer."
  (declare (indent 0) (debug t))
  `(agile-gtd-org-ql-test-with-sandbox
    (let* ((file (expand-file-name "hide-today-fixtures.org" org-directory))
           (buffer nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert (agile-gtd-agenda-test-hide-today-data)))
            (setq buffer (find-file-noselect file))
            (with-current-buffer buffer
              (org-mode)
              (agile-gtd-enable)
              (org-set-regexps-and-options)
              ,@body))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agile-gtd-next-actions-default-includes-today-and-overdue ()
  "hide-today nil (default): scheduled/deadline today and overdue items appear."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((headings (agile-gtd-org-ql-test-headings
                     buffer (agile-gtd-agenda-query-next-actions))))
      (should (member "Plain next action" headings))
      (should (member "Scheduled today" headings))
      (should (member "Deadline today" headings))
      (should (member "Scheduled yesterday (overdue)" headings))
      (should (member "Deadline yesterday (overdue)" headings)))))

(ert-deftest agile-gtd-next-actions-default-excludes-future-scheduled ()
  "hide-today nil (default): future-scheduled items are excluded."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((headings (agile-gtd-org-ql-test-headings
                     buffer (agile-gtd-agenda-query-next-actions))))
      (should-not (member "Scheduled tomorrow (future)" headings)))))

(ert-deftest agile-gtd-next-actions-hide-today-excludes-all-scheduled-and-deadline ()
  "hide-today t: all scheduled and deadline items are excluded."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((headings (agile-gtd-org-ql-test-headings
                     buffer (agile-gtd-agenda-query-next-actions nil nil t))))
      (should (member "Plain next action" headings))
      (should-not (member "Scheduled today" headings))
      (should-not (member "Deadline today" headings))
      (should-not (member "Scheduled yesterday (overdue)" headings))
      (should-not (member "Deadline yesterday (overdue)" headings))
      (should-not (member "Scheduled tomorrow (future)" headings)))))

(ert-deftest agile-gtd-next-actions-excludes-someday ()
  "Someday items are excluded regardless of hide-today."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((default-headings (agile-gtd-org-ql-test-headings
                              buffer (agile-gtd-agenda-query-next-actions)))
          (hide-today-headings (agile-gtd-org-ql-test-headings
                                 buffer (agile-gtd-agenda-query-next-actions nil nil t))))
      (should-not (member "Someday item" default-headings))
      (should-not (member "Someday item" hide-today-headings)))))

(ert-deftest agile-gtd-next-actions-default-shows-tickler-today ()
  "hide-today nil (default): tickler items scheduled today appear (not someday)."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((headings (agile-gtd-org-ql-test-headings
                     buffer (agile-gtd-agenda-query-next-actions))))
      (should (member "Tickler today" headings))
      (should-not (member "Tickler future" headings)))))

(ert-deftest agile-gtd-next-actions-hide-today-excludes-tickler ()
  "hide-today t: tickler items are excluded (they are scheduled)."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((headings (agile-gtd-org-ql-test-headings
                     buffer (agile-gtd-agenda-query-next-actions nil nil t))))
      (should-not (member "Tickler today" headings))
      (should-not (member "Tickler future" headings)))))

(ert-deftest agile-gtd-next-actions-includes-habits ()
  "Habit items are included (filtered by priority like everything else)."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((headings (agile-gtd-org-ql-test-headings
                     buffer (agile-gtd-agenda-query-next-actions))))
      (should (member "Habit item" headings)))))

(ert-deftest agile-gtd-next-actions-default-matches-explicit-nil ()
  "Calling with no hide-today arg is equivalent to explicit nil."
  (agile-gtd-agenda-test-hide-today-data-do
    (let ((default-headings (agile-gtd-org-ql-test-headings
                             buffer (agile-gtd-agenda-query-next-actions)))
          (explicit-headings (agile-gtd-org-ql-test-headings
                              buffer (agile-gtd-agenda-query-next-actions nil nil nil))))
      (should (equal default-headings explicit-headings)))))

;;; agile-gtd-agenda-test.el ends here
