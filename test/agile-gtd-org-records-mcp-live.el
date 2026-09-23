;;; agile-gtd-org-records-mcp-live.el --- org-records-mcp view keys against the real Org corpus -*- lexical-binding: t; -*-

;; agile-gtd configures org-records-mcp as it loads: one view per key, the rank sort,
;; the computed fields and the file scope.  The package's own suite proves the
;; keys on a fixture; this proves them on the loaded configuration, with every
;; registered project, and checks that a key answers what the agenda block it
;; mirrors shows.
;;
;; Read-only.  It runs views and renders an agenda; it writes nothing.
;;
;;   emacs -q --batch -l ~/.config/doom/test/bootstrap.el \\
;;         -l ~/.config/doom/test/agile-gtd-org-records-mcp-live.el

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'org-ql)
(require 'agile-gtd)
(require 'org-records-mcp)

(defvar agm-failures 0)
(defun agm-check (label ok &optional detail)
  (message "%s %-58s %s" (if ok "PASS" "FAIL") label (or detail ""))
  (unless ok (setq agm-failures (1+ agm-failures))))

(agm-check "package loads" (and (featurep 'agile-gtd) (featurep 'org-records-mcp)))


;;; What agile-gtd sets

(defvar agm-areas (agile-gtd-areas))
(defvar agm-view-names (mapcar #'car org-records-mcp-views))

(agm-check "one view per key: 13 per area, plus inbox and tangling"
           (= (length org-records-mcp-views) (+ (* 13 (length agm-areas)) 2))
           (format "%d views, %d areas" (length org-records-mcp-views) (length agm-areas)))

(let ((missing
       (cl-remove-if
        (lambda (key) (memq key agm-view-names))
        (append '(inbox tangling next next-today private-stuck work-backlog-sprint)
                (mapcan (lambda (tag)
                          (mapcar (lambda (view) (intern (format "%s-%s" tag view)))
                                  '("next" "next-today" "backlog" "backlog-today"
                                    "stuck")))
                        (mapcar #'agile-gtd--project-tag
                                (agile-gtd-project-records)))))))
  (agm-check "every registered project has its keys"
             (null missing)
             (if missing (format "MISSING: %S" (seq-take missing 5))
               (format "%d projects" (length (agile-gtd-project-records))))))

(agm-check "sort function is the rank"
           (eq org-records-mcp-query-sort-fn #'agile-gtd--item-rank<))

(dolist (field '(rank parent-priority blocked))
  (agm-check (format "computed field %s is set" field)
             (functionp (alist-get field org-records-mcp-computed-fields))))

(agm-check "org-records-mcp-allowed-files is nil" (null org-records-mcp-allowed-files))
(agm-check "org-records-mcp-file-scope-override is t" (eq org-records-mcp-file-scope-override t))


;;; The org-view description

;; The description is built when the tools are registered, so this reads the
;; one a connecting client would get, not the catalogue function's return.
(let ((description
       (progn
         (org-records-mcp-enable)
         (unwind-protect
             (plist-get (gethash "org-view"
                                 (gethash org-records-mcp--server-id mcp-server-lib--tools))
                        :description)
           (org-records-mcp-disable)))))
  (agm-check "org-view description states the key grammar"
             (and description
                  (string-search "[<area>-]<view>[-<range>]" description)))
  (agm-check "org-view description has no per-view lines"
             (and description
                  (not (string-match-p "^ +[a-z-]+ - takes " description)))))


;;; Keys against the corpus

(defun agm-run (key &optional filter range)
  "Run the view KEY and return its parsed JSON, or the error it signals."
  (condition-case err
      (json-parse-string (org-records-mcp--tool-view key filter range)
                         :object-type 'alist :array-type 'list
                         :false-object :json-false)
    (error err)))

(defun agm-ranks (result)
  "Return the rank of every node in RESULT, in order."
  (mapcar (lambda (node) (alist-get 'rank (alist-get 'computed node)))
          (alist-get 'children result)))

(dolist (key '("next-today" "private-next" "oebb-next" "work-backlog-sprint" "stuck"))
  (let* ((result (agm-run key))
         (ok (and (consp result) (assq 'children result)))
         (ranks (and ok (agm-ranks result))))
    (agm-check (format "%s runs and returns children" key)
               ok
               (if ok (format "%d nodes" (length (alist-get 'children result)))
                 (format "%S" result)))
    (agm-check (format "%s is in rank order" key)
               (and ok (seq-every-p #'numberp ranks)
                    (or (null ranks) (apply #'<= ranks)))
               (format "%S" (seq-take ranks 8)))))

(defun agm-refused-p (result)
  "Non-nil when RESULT is the tool error org-records-mcp refuses a call with."
  (eq (car-safe result) 'mcp-server-lib-tool-error))

(agm-check "a key refuses a filter" (agm-refused-p (agm-run "oebb-next" "oebb")))
(agm-check "a key refuses a range" (agm-refused-p (agm-run "oebb-next" nil "sprint")))
(agm-check "an unknown key is refused" (agm-refused-p (agm-run "no-such-area-next")))


;;; A key and its agenda block return the same items

;; The w o block hides what the day block above it carries; the key does not.
;; So the block has to hold exactly what the hide-today form of the key's own
;; query selects, and all of that has to be in the key's answer.

(defun agm-block-items (command header)
  "Render agenda COMMAND and return (FILE . HEADING) of every item under HEADER."
  (let ((org-agenda-window-setup 'current-window)
        items)
    (unwind-protect
        (save-window-excursion
          (org-agenda nil command)
          (with-current-buffer org-agenda-buffer-name
            (goto-char (point-min))
            (when (search-forward header nil t)
              (while (not (eobp))
                (when-let ((m (get-text-property (point) 'org-hd-marker)))
                  (push (org-with-point-at m
                          (cons (buffer-file-name (buffer-base-buffer))
                                (org-get-heading t t t t)))
                        items))
                (forward-line 1)))))
      (when-let ((b (get-buffer org-agenda-buffer-name))) (kill-buffer b)))
    (delete-dups (nreverse items))))

(let* ((area (agile-gtd--area "oebb"))
       (block (agm-block-items "wo" "Next Actions"))
       (expected (org-ql-select (org-agenda-files)
                     (agile-gtd-agenda-query-next-actions
                      (plist-get area :filter) (plist-get area :next-range) t)
                   :action (lambda () (cons (buffer-file-name (buffer-base-buffer))
                                            (org-get-heading t t t t)))))
       (key (mapcar (lambda (node)
                      (cons (expand-file-name (alist-get 'file node))
                            (alist-get 'title node)))
                    (alist-get 'children (agm-run "oebb-next"))))
       (block-only (cl-set-difference block expected :test #'equal))
       (query-only (cl-set-difference expected block :test #'equal))
       (not-in-key (cl-set-difference block key :test #'equal)))
  (agm-check "w o next actions match the hide-today query"
             (and block (null block-only) (null query-only))
             (format "%d block items%s%s" (length block)
                     (if block-only (format " — BLOCK ONLY: %S" (seq-take block-only 3)) "")
                     (if query-only (format " — QUERY ONLY: %S" (seq-take query-only 3)) "")))
  (agm-check "oebb-next holds every w o next action"
             (and block (null not-in-key))
             (format "%d key items, %d block items%s" (length key) (length block)
                     (if not-in-key (format " — MISSING: %S" (seq-take not-in-key 3)) ""))))

(message "\n%s: %d check(s) failed"
         (if (zerop agm-failures) "OK" "FAILURES") agm-failures)
(kill-emacs (if (zerop agm-failures) 0 1))

;;; agile-gtd-org-records-mcp-live.el ends here
