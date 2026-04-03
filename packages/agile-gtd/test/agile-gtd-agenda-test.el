;;; agile-gtd-agenda-test.el --- Tests for agile-gtd agenda query functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-modern)
(require 'org-ql)
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

(ert-deftest agile-gtd-agenda-query-today-items-returns-sexp ()
  "Today items query returns a well-formed sexp."
  (let ((query (agile-gtd-agenda-query-today-items)))
    (should (listp query))
    (should (eq 'and (car query))))
  (let ((filtered (agile-gtd-agenda-query-today-items '(agile-gtd-work))))
    (should (listp filtered))
    (should (eq 'and (car filtered)))))

(ert-deftest agile-gtd-agenda-query-next-actions-returns-sexp ()
  "Next actions query returns a well-formed sexp with and without filter."
  (let ((query (agile-gtd-agenda-query-next-actions)))
    (should (listp query))
    (should (eq 'and (car query))))
  (let ((filtered (agile-gtd-agenda-query-next-actions '(agile-gtd-work))))
    (should (listp filtered))
    ;; Filtered query wraps with an outer `and'
    (should (eq 'and (car filtered)))))

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
     ;; No-priority default rank 45 should be highest
     (should (= 45 (cdr (assoc "No priority private" ranked)))))))

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

;;; agile-gtd-agenda-test.el ends here
