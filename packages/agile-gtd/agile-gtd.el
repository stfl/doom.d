;;; agile-gtd.el --- Agile GTD workflow for Org -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; URL: https://github.com/stfl/doom.d
;; Package-Requires: ((emacs "30.2") (dash "2.19.1") (org-modern "1.6") (org-ql "0.8") (org-super-agenda "1.3") (ts "0.3"))
;; Keywords: outlines, calendar, tools

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-element)
(require 'org-id)
(require 'org-ql)
(require 'org-super-agenda)
(require 'ts)

(defvar org-modern-priority)

(defgroup agile-gtd nil
  "Agile and GTD helpers for Org mode."
  :group 'org)

(defcustom agile-gtd-priority-highest ?A
  "Highest priority used by Agile GTD."
  :type 'character
  :group 'agile-gtd)

(defcustom agile-gtd-priority-default ?E
  "Default priority used by Agile GTD."
  :type 'character
  :group 'agile-gtd)

(defcustom agile-gtd-priority-lowest ?I
  "Lowest priority used by Agile GTD."
  :type 'character
  :group 'agile-gtd)

(defcustom agile-gtd-priority-symbol-alist
  '((?A . "⛔")
    (?B . "▲")
    (?C . "𐱄")
    (?D . "ᐱ")
    (?E . "Ⲷ")
    (?F . "ᐯ")
    (?G . "𐠠")
    (?H . "▼")
    (?I . "҉"))
  "Symbols shown by org-modern for GTD priorities."
  :type '(alist :key-type character :value-type string)
  :group 'agile-gtd)

(defcustom agile-gtd-priority-face-alist
  '((?A . (:foreground "red3" :weight bold :height 0.95))
    (?B . (:foreground "OrangeRed2" :weight bold))
    (?C . (:foreground "DarkOrange2" :weight bold))
    (?D . (:foreground "gold3" :weight bold))
    (?E . (:foreground "OliveDrab1" :weight bold))
    (?F . (:foreground "SpringGreen3" :weight bold))
    (?G . (:foreground "cyan4" :weight bold))
    (?H . (:foreground "DeepSkyBlue4" :weight bold))
    (?I . (:foreground "LightSteelBlue3" :weight bold)))
  "Faces used for GTD priorities."
  :type '(alist :key-type character :value-type plist)
  :group 'agile-gtd)

(defcustom agile-gtd-todo-keywords
  '((sequence
     "TODO(t)"
     "NEXT(n)"
     "WAIT(w)"
     "PROJ(p)"
     "EPIC(e)"
     "|"
     "DONE(d@)"
     "IDEA(i)"
     "KILL(k@)"))
  "TODO keyword sequence used by Agile GTD."
  :type 'sexp
  :group 'agile-gtd)

(defcustom agile-gtd-todo-repeat-to-state "NEXT"
  "State repeated tasks should move to."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-someday-tag "SOMEDAY"
  "Tag used for someday items."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-habit-tag "HABIT"
  "Tag used for habits."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-lastmile-tag "LASTMILE"
  "Tag used for nearly finished tasks."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-work-tag "#work"
  "Tag used for work items."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-personal-tag "#personal"
  "Tag used for personal items."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-customers nil
  "List of configured work customers.
Each entry is a plist with:
  :tag  - org tag string identifying this customer (required)
  :name - display name (defaults to value of :tag)
  :file - org file relative to `org-directory' (defaults to :tag \".org\")
  :key  - single character for agenda key binding and tag-alist (required)"
  :type '(repeat (plist :key-type keyword :value-type sexp))
  :group 'agile-gtd)

(defcustom agile-gtd-inbox-file "inbox.org"
  "Inbox file relative to `org-directory'."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-inbox-orgzly-file "inbox-orgzly.org"
  "Orgzly inbox file relative to `org-directory'."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-todo-file "todo.org"
  "Todo file relative to `org-directory'."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-project-files nil
  "Project files relative to `org-directory'."
  :type '(repeat string)
  :group 'agile-gtd)

(defcustom agile-gtd-diary-file "diary.org"
  "Diary file relative to `org-directory'."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-someday-files-glob "gtd/someday/*.org"
  "Glob relative to `org-directory' used for someday refile targets."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-inbox-heading "Inbox"
  "Heading used for inbox captures."
  :type 'string
  :group 'agile-gtd)

(defcustom agile-gtd-inbox-tags '("#inbox" "inbox")
  "Tags treated as inbox items in the agenda."
  :type '(repeat string)
  :group 'agile-gtd)

(defcustom agile-gtd-max-priority-group nil
  "Highest visible priority group in agenda commands.

When nil, derive it from `agile-gtd-priority-default'."
  :type '(choice (const :tag "Derived from default" nil)
          character)
  :group 'agile-gtd)

(defcustom agile-gtd-backlog-priority-threshold nil
  "Priority threshold after which backlog items count as someday.

When nil, derive it from `agile-gtd-priority-default'."
  :type '(choice (const :tag "Derived from default" nil)
          character)
  :group 'agile-gtd)

(defcustom agile-gtd-enable-agenda-files t
  "Whether `agile-gtd-enable' should manage `org-agenda-files'."
  :type 'boolean
  :group 'agile-gtd)

(defcustom agile-gtd-enable-refile-targets t
  "Whether `agile-gtd-enable' should manage `org-refile-targets'."
  :type 'boolean
  :group 'agile-gtd)

(defcustom agile-gtd-enable-org-modern-visuals t
  "Whether Agile GTD should configure org-modern priority visuals."
  :type 'boolean
  :group 'agile-gtd)

(defface agile-gtd-todo-active
  '((t (:inherit (bold font-lock-constant-face org-todo))))
  "Face for active TODO items."
  :group 'agile-gtd)

(defface agile-gtd-todo-idea
  '((t (:inherit (bold font-lock-constant-face org-todo))))
  "Face for idea items."
  :group 'agile-gtd)

(defface agile-gtd-todo-project
  '((t (:inherit (bold font-lock-doc-face org-todo))))
  "Face for projects."
  :group 'agile-gtd)

(defface agile-gtd-todo-epic
  '((t (:inherit (bold org-cite org-todo))))
  "Face for epics."
  :group 'agile-gtd)

(defface agile-gtd-todo-onhold
  '((t (:inherit (bold warning org-todo))))
  "Face for waiting items."
  :group 'agile-gtd)

(defface agile-gtd-todo-next
  '((t (:inherit (bold font-lock-keyword-face org-todo))))
  "Face for next actions."
  :group 'agile-gtd)

(defface agile-gtd-todo-cancel
  '((t (:inherit (bold org-done) :foreground "IndianRed3")))
  "Face for cancelled items."
  :group 'agile-gtd)

(defun agile-gtd--project-keyword ()
  "Return the GTD project keyword."
  "PROJ")

(defun agile-gtd--action-keywords ()
  "Return the GTD action keywords."
  '("NEXT" "WAIT"))

(defun agile-gtd-project-keyword ()
  "Return the public GTD project keyword."
  (agile-gtd--project-keyword))

(defun agile-gtd-action-keywords ()
  "Return the public GTD action keywords."
  (copy-sequence (agile-gtd--action-keywords)))

(defun agile-gtd--priority-range ()
  "Return the configured priority range."
  (number-sequence agile-gtd-priority-highest agile-gtd-priority-lowest))

(defun agile-gtd--priority-in-range-p (priority)
  "Return non-nil when PRIORITY is within the configured priority range."
  (and (characterp priority)
       (<= agile-gtd-priority-highest priority agile-gtd-priority-lowest)))

(defun agile-gtd--validate-configuration ()
  "Validate the current Agile GTD configuration."
  (unless (<= agile-gtd-priority-highest
              agile-gtd-priority-default
              agile-gtd-priority-lowest)
    (error "Agile GTD priorities must satisfy highest <= default <= lowest"))
  (dolist (priority (delq nil (list agile-gtd-max-priority-group
                                    agile-gtd-backlog-priority-threshold)))
    (unless (agile-gtd--priority-in-range-p priority)
      (error "Priority %s is outside the configured Agile GTD range"
             priority))))

(defun agile-gtd--customer-tag (c)
  "Return the tag for customer C."
  (plist-get c :tag))

(defun agile-gtd--customer-name (c)
  "Return the display name for customer C."
  (or (plist-get c :name) (plist-get c :tag)))

(defun agile-gtd--customer-file (c)
  "Return the org file for customer C."
  (or (plist-get c :file) (concat (plist-get c :tag) ".org")))

(defun agile-gtd--customer-key (c)
  "Return the key character for customer C."
  (plist-get c :key))

(defun agile-gtd--workflow-tag-alist ()
  "Return the workflow tag definitions managed by Agile GTD."
  `((:startgrouptag)
    ("Process" . nil)
    (:grouptags)
    (,agile-gtd-someday-tag . ?S)
    (,agile-gtd-habit-tag . ?H)
    (,agile-gtd-lastmile-tag . ?L)
    (:endgrouptag)
    (:startgrouptag)
    ("Areas" . nil)
    (:grouptags)
    (,agile-gtd-work-tag . ?$)
    (,agile-gtd-personal-tag . ?_)
    (:endgrouptag)))

(defun agile-gtd--merge-tag-alist (current additions)
  "Merge ADDITIONS into CURRENT without overwriting existing tag names."
  (let ((result (copy-tree current)))
    (dolist (entry additions result)
      (unless (assoc (car entry) result)
        (setq result (append result (list entry)))))))

(defun agile-gtd--workflow-tag-names ()
  "Return the workflow tag names managed by Agile GTD."
  (list "Process"
        agile-gtd-someday-tag
        agile-gtd-habit-tag
        agile-gtd-lastmile-tag
        "Areas"
        agile-gtd-work-tag
        agile-gtd-personal-tag))

(defun agile-gtd--list-prefix-p (prefix list)
  "Return non-nil when PREFIX matches the start of LIST."
  (and (<= (length prefix) (length list))
       (cl-every #'equal prefix (cl-subseq list 0 (length prefix)))))

(defun agile-gtd--delete-sublist (sublist list)
  "Delete all SUBLIST occurrences from LIST."
  (let ((result nil)
        (tail list)
        (sublist-length (length sublist)))
    (while tail
      (if (agile-gtd--list-prefix-p sublist tail)
          (setq tail (nthcdr sublist-length tail))
        (push (pop tail) result)))
    (nreverse result)))

(defun agile-gtd--replace-by-key (current additions)
  "Replace entries in CURRENT whose key matches an entry in ADDITIONS."
  (let ((keys (mapcar #'car additions)))
    (append
     (cl-remove-if (lambda (item)
                     (member (car-safe item) keys))
                   current)
     additions)))

(defun agile-gtd--priority-symbols ()
  "Return org-modern priority symbols for the configured range."
  (--keep (when-let ((symbol (alist-get it agile-gtd-priority-symbol-alist)))
            (cons it symbol))
          (agile-gtd--priority-range)))

(defun agile-gtd--priority-faces ()
  "Return `org-priority-faces' data for the configured range."
  (--keep (when-let ((face (alist-get it agile-gtd-priority-face-alist)))
            (append (list it) face))
          (agile-gtd--priority-range)))

(defun agile-gtd--priority-prompt-choices ()
  "Return the priority choices used in capture templates."
  (mapconcat (lambda (priority)
               (format "[#%c]" priority))
             (agile-gtd--priority-range)
             " |"))

(defun agile-gtd--expand-org-path (file)
  "Expand FILE relative to `org-directory'."
  (expand-file-name file org-directory))

(defun agile-gtd--customer-files ()
  "Return the list of customer files derived from `agile-gtd-customers'."
  (mapcar #'agile-gtd--customer-file agile-gtd-customers))

(defun agile-gtd--agenda-files ()
  "Return the agenda files managed by Agile GTD."
  (mapcar #'agile-gtd--expand-org-path
          (cl-remove-duplicates
           (append (list agile-gtd-inbox-file
                         agile-gtd-inbox-orgzly-file
                         agile-gtd-todo-file)
                   agile-gtd-project-files
                   (agile-gtd--customer-files))
           :test #'equal)))

(defun agile-gtd--someday-files ()
  "Return the someday files used for refiling."
  (file-expand-wildcards (agile-gtd--expand-org-path agile-gtd-someday-files-glob)))

(defun agile-gtd--current-max-priority-group ()
  "Return the currently active maximum priority group."
  (or agile-gtd-max-priority-group
      (max agile-gtd-priority-highest (1- agile-gtd-priority-default))))

(defun agile-gtd--current-backlog-priority-threshold ()
  "Return the currently active backlog threshold."
  (or agile-gtd-backlog-priority-threshold
      (min agile-gtd-priority-lowest (+ agile-gtd-priority-default 2))))

(defun agile-gtd--capture-template-project ()
  "Return the project capture template."
  (concat "* PROJ %^{PRIORITY||"
          (agile-gtd--priority-prompt-choices)
          " }%^{Title}\n"
          ":PROPERTIES:\n"
          ":ID:       %(org-id-new)\n"
          ":CREATED:  %U\n"
          ":END:\n\n"
          "~Goal:~ %^{Goal}\n\n"
          "** NEXT %^{Next Action}\n"
          ":PROPERTIES:\n"
          ":CREATED:  %U\n"
          ":END:\n\n"
          "%?\n"))

(defun agile-gtd--protocol-description (description)
  "Normalize DESCRIPTION for protocol capture links."
  (let ((text (or description "")))
    (setq text (replace-regexp-in-string "\\[" "(" text))
    (replace-regexp-in-string "\\]" ")" text)))

(defun agile-gtd--capture-templates ()
  "Return the Agile GTD capture templates."
  `(("n" "capture to inbox" entry
     (file+headline ,(agile-gtd--expand-org-path agile-gtd-inbox-file) ,agile-gtd-inbox-heading)
     "* TODO %^{Task}\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-uuid)\n:END:\n\n%?\n"
     :empty-lines-after 1)
    ("p" "Project" entry
     (file ,(agile-gtd--expand-org-path agile-gtd-inbox-file))
     ,(agile-gtd--capture-template-project)
     :empty-lines-after 1)
    ("s" "scheduled" entry
     (file ,(agile-gtd--expand-org-path agile-gtd-inbox-file))
     "* NEXT %^{Task}\nSCHEDULED: %^{Scheduled}t\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?\n"
     :empty-lines-after 1)
    ("S" "deadline" entry
     (file ,(agile-gtd--expand-org-path agile-gtd-inbox-file))
     "* NEXT %^{Task}\nDEADLINE: %^{Deadline}t\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?\n"
     :empty-lines-after 1)
    ("P" "Protocol" entry
     (file ,(agile-gtd--expand-org-path agile-gtd-inbox-file))
     ,(concat "* %^{Title}\n"
              "Source: [[%:link][%(agile-gtd--protocol-description \"%:description\")]]\n"
              ":PROPERTIES:\n"
              ":CREATED: %U\n"
              ":END:\n"
              "#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
     :empty-lines-after 1)
    ("L" "Protocol Link" entry
     (file ,(agile-gtd--expand-org-path agile-gtd-inbox-file))
     "* [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
     :empty-lines-after 1)))

(defun agile-gtd--stuck-projects-setting ()
  "Return the `org-stuck-projects' setting for Agile GTD."
  (list (format "-%s/+%s" agile-gtd-someday-tag (agile-gtd--project-keyword))
        (agile-gtd--action-keywords)
        nil
        ""))

(defun agile-gtd--prio-rank (priority)
  "Return numeric rank for PRIORITY character, or nil for nil input."
  (when priority
    (pcase priority
      (?A 1)
      (?B 11)
      (?C 21)
      (?D 31)
      (?E 41)
      (?F 51)
      (?G 61)
      (?H 71)
      (?I 81))))

(defun agile-gtd--deadline-rank (days)
  "Return numeric rank for DAYS until deadline (integer)."
  (cond
   ((< days 0)    days)
   ((= days 0)    -1)
   ((<= days 2)    0)
   ((<= days 5)   10)
   ((<= days 7)   20)
   ((<= days 11)  30)
   ((<= days 14)  40)
   ((<= days 21)  50)
   ((<= days 30)  60)
   ((<= days 60)  70)
   (t            1000)))

(defconst agile-gtd--rank-inf 99999)

(defun agile-gtd--backlog-rank (prio parent-prio dl-delta)
  "Return numeric backlog rank.
PRIO and PARENT-PRIO are priority characters or nil.
DL-DELTA is integer days until deadline or nil."
  (let* ((own      (or (agile-gtd--prio-rank prio)        agile-gtd--rank-inf))
         (par      (or (agile-gtd--prio-rank parent-prio) agile-gtd--rank-inf))
         (dl       (if dl-delta (agile-gtd--deadline-rank dl-delta) agile-gtd--rank-inf))
         (combined (min own par dl)))
    (if (>= combined agile-gtd--rank-inf) 45 combined)))

(defconst agile-gtd--priority-deadline-days
  '((?A . 2)
    (?B . 5)
    (?C . 7)
    (?D . 11)
    (?E . 14)
    (?F . 21)
    (?G . 30)
    (?H . 60))
  "Maximum days-until-deadline for each priority level.")

(defun agile-gtd--deadline-window (priority)
  "Return the deadline window in days for PRIORITY (hard-coded table)."
  (or (alist-get priority agile-gtd--priority-deadline-days) 0))

(defun agile-gtd--priority-or-default ()
  "Return the priority at point or the default fallback."
  (or (org-element-property :priority (org-element-at-point))
      (+ 0.5 org-priority-default)))

(defun agile-gtd--direct-parent-priority ()
  "Return the direct parent heading's priority character, or nil."
  (save-excursion
    (when (org-up-heading-safe)
      (org-element-property :priority (org-element-at-point)))))

(defun agile-gtd--parent-project-priority-or-default (marker)
  "Return the parent project priority for MARKER."
  (org-with-point-at marker
    (cl-loop minimize (when (equal (agile-gtd--project-keyword)
                                   (nth 2 (org-heading-components)))
                        (agile-gtd--priority-or-default))
             while (and (not (equal (agile-gtd--project-keyword)
                                    (nth 2 (org-heading-components))))
                        (org-up-heading-safe)))))

(defun agile-gtd--project-priority= (marker priority)
  "Return non-nil when MARKER belongs to a project with PRIORITY."
  (let ((project-priority (agile-gtd--parent-project-priority-or-default marker)))
    (and project-priority
         (= project-priority priority))))

(defun agile-gtd-priority-groups ()
  "Return priority-based org-super-agenda groups."
  (append
   `((:tag ,agile-gtd-someday-tag :order 90))
   (mapcar (lambda (priority)
             (let ((priority-string (char-to-string priority)))
               `(:name ,(format "[#%s] Priority %s" priority-string priority-string)
                 :priority ,priority-string
                 :order ,priority)))
           (agile-gtd--priority-range))
   `((:name "Default Priority"
      :not :priority))))

(defun agile-gtd-ancestor-priority-groups ()
  "Return ancestor-priority org-super-agenda groups."
  (append
   `((:name "Tickler"
      :and (:scheduled t :tag ,agile-gtd-someday-tag)
      :order ,(1+ org-priority-lowest))
     (:name "Someday"
      :tag ,agile-gtd-someday-tag
      :order ,(+ 2 org-priority-lowest)))
   (mapcar
    (lambda (priority)
      (let ((priority-string (char-to-string priority))
            (until-date (ts-format "%Y-%m-%d"
                                   (ts-adjust 'day
                                              (agile-gtd--deadline-window priority)
                                              (ts-now)))))
        `(:name ,(format "[#%s] Priority %s" priority-string priority-string)
          :deadline (before ,until-date)
          :scheduled (before ,until-date)
          :priority ,priority-string
          :pred ((lambda (item)
                   (agile-gtd--project-priority=
                    (org-find-text-property-in-string 'org-marker item)
                    ,priority)))
          :order ,priority)))
    (agile-gtd--priority-range))
   `((:name "Default Priority (Rest)"
      :anything t
      :order ,(+ 0.5 org-priority-default)))))

(defun agile-gtd--today-groups ()
  "Return the org-super-agenda groups used by the today agenda."
  `((:time-grid t :order 0)
    (:name "Tickler" :tag ,agile-gtd-someday-tag :order 20)
    (:name "Habits" :tag ,agile-gtd-habit-tag :habit t :order 90)
    (:name "Today" :anything t :order 10)))

(defun agile-gtd--agenda-day (&optional tag-filter-preset)
  "Return the base agenda block used by the daily view.
TAG-FILTER-PRESET, when non-nil, is a list of strings for
`org-agenda-tag-filter-preset' (e.g. \\='(\"+#work\") or \\='(\"-#work\"))."
  `(agenda "Agenda"
    ((org-agenda-use-time-grid t)
     (org-deadline-warning-days 0)
     (org-agenda-span '1)
     (org-super-agenda-groups ',(agile-gtd--today-groups))
     (org-agenda-start-day (org-today))
     ,@(when tag-filter-preset
         `((org-agenda-tag-filter-preset ',tag-filter-preset))))))

(defun agile-gtd--someday-habit ()
  "Return an org-ql sexp matching someday and habit items."
  `(or (tags ,agile-gtd-someday-tag ,agile-gtd-habit-tag)
       (habit)))

(defun agile-gtd-not-someday-habit ()
  "Return an org-ql sexp excluding someday and habit items."
  `(not ,(agile-gtd--someday-habit)))

(defun agile-gtd--prio-deadline>= (priority)
  "Return an org-ql sexp for items at or above PRIORITY urgency."
  `(and (or (priority >= (char-to-string ,priority))
            (and ,(> (agile-gtd--current-max-priority-group) org-priority-default)
                 (not (priority)))
            (agile-gtd-parent-prio <= ,priority)
            (agile-gtd-deadline-prio <= ,priority))))

(defun agile-gtd-agenda-query-actions-prio-higher (priority)
  "Return an org-ql sexp for action items at or above PRIORITY."
  `(and (todo ,@(agile-gtd--action-keywords))
        ,(agile-gtd--prio-deadline>= priority)
        (not ,(agile-gtd--someday-habit))
        (not (ancestors (deadline :to 0)))
        (not (deadline :to 0))
        (not (scheduled))))

(defun agile-gtd-agenda-query-today-items (&optional tag-filter)
  "Return org-ql sexp for items due or active today.
TAG-FILTER, when non-nil, is `and'-ed in to narrow by tag."
  (let ((base `(and (not (done))
                    (or (agile-gtd-habit)
                        (deadline :to today)
                        (scheduled :to today)
                        (ts-active :on today)))))
    (if tag-filter `(and ,base ,tag-filter) base)))

(defun agile-gtd-agenda-query-next-actions (&optional tag-filter priority)
  "Return org-ql sexp for next actions at or above PRIORITY.
TAG-FILTER, when non-nil, is `and'-ed in to narrow by tag.
PRIORITY defaults to `agile-gtd--current-max-priority-group'."
  (let* ((prio (or priority (agile-gtd--current-max-priority-group)))
         (base `(and (todo ,@(agile-gtd--action-keywords))
                     ,(agile-gtd--prio-deadline>= prio)
                     (not ,(agile-gtd--someday-habit))
                     (not (ancestors (deadline :to 0)))
                     (not (deadline :to 0))
                     (not (scheduled)))))
    (if tag-filter `(and ,base ,tag-filter) base)))

(defun agile-gtd-agenda-query-inbox ()
  "Return org-ql sexp for inbox items."
  `(and (not (done))
        (tags ,@agile-gtd-inbox-tags)))

(defun agile-gtd-agenda-query-backlog (&optional tag-filter)
  "Return org-ql sexp for backlog (projects and standalone next actions).
TAG-FILTER, when non-nil, is `and'-ed in to narrow by tag."
  (let ((base `(and (or (todo ,(agile-gtd--project-keyword))
                        (agile-gtd-standalone-next))
                    (not (agile-gtd-habit)))))
    (if tag-filter `(and ,base ,tag-filter) base)))

(defun agile-gtd-agenda-query-stuck-projects (&optional tag-filter)
  "Return org-ql sexp for stuck projects.
TAG-FILTER, when non-nil, is `and'-ed in to narrow by tag."
  (if tag-filter
      `(and (agile-gtd-stuck-proj) ,tag-filter)
    '(agile-gtd-stuck-proj)))

(defun agile-gtd--customer-agenda-commands ()
  "Return agenda commands for each configured customer."
  (mapcar
   (lambda (customer)
     (let* ((tag  (agile-gtd--customer-tag customer))
            (name (agile-gtd--customer-name customer))
            (key  (char-to-string (agile-gtd--customer-key customer)))
            (filt `(tags ,tag)))
       `(,(concat "w" key) ,(format "%s Agenda" name)
         (,(agile-gtd--agenda-day (list (concat "+" tag)))
          (org-ql-block ,(agile-gtd-agenda-query-stuck-projects filt)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")))
          (org-ql-block ,(agile-gtd-agenda-query-next-actions filt)
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-groups ',(agile-gtd-ancestor-priority-groups))))))))
   agile-gtd-customers))

(defun agile-gtd--agenda-custom-commands ()
  "Return the Agile GTD agenda commands."
  `(("i" "Inbox"
     ((org-ql-block `(and (not (done))
                          (tags ,@agile-gtd-inbox-tags))
                    ((org-ql-block-header "Inbox")
                     (org-super-agenda-groups '((:auto-property "CREATED")))))))
    ("a" "Main Agenda"
     (,(agile-gtd--agenda-day)
      (org-ql-block ,(agile-gtd-agenda-query-stuck-projects)
                    ((org-ql-block-header "Stuck Projects")
                     (org-super-agenda-header-separator "")))
      (org-ql-block ,(agile-gtd-agenda-query-next-actions)
                    ((org-ql-block-header "Next Actions")
                     (org-super-agenda-groups ',(agile-gtd-ancestor-priority-groups))))))
    ("A" "Agenda Weekly"
     ((agenda ""
              ((org-agenda-span 'week)
               (org-agenda-start-on-weekday 1)))))
    ("l" "Agenda Weekly with Log"
     ((agenda ""
              ((org-agenda-span 'week)
               (org-agenda-start-on-weekday 1)
               (org-agenda-archives-mode t)
               (org-agenda-start-with-log-mode '(closed))
               (org-agenda-show-log 'logcheck)
               (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^.*DONE "))))))
    ("r" . "Review")
    ("rc" "Close open NEXT Actions and WAIT"
     ((org-ql-block `(and (todo ,@(agile-gtd--action-keywords))
                          (not (tags ,agile-gtd-someday-tag ,agile-gtd-habit-tag))
                          (not (agile-gtd-habit))
                          (or (not (deadline))
                              (deadline :to "+30")
                              (ancestors (deadline :to "+30")))
                          (or (not (scheduled))
                              (scheduled :to "+30")))
                    ((org-super-agenda-header-separator "")
                     (org-deadline-warning-days 30)
                     (org-super-agenda-groups ',(agile-gtd-ancestor-priority-groups))
                     (org-ql-block-header "Something to do")))
      (org-ql-block (agile-gtd-agenda-query-stuck-projects)
                    ((org-ql-block-header "Stuck Projects")
                     (org-super-agenda-header-separator "")
                     (org-super-agenda-groups ',(agile-gtd-priority-groups))))))
    ("rs" "Stuck Projects"
     ((org-ql-block '(agile-gtd-stuck-proj)
                    ((org-ql-block-header "Stuck Projects")
                     (org-super-agenda-header-separator "")
                     (org-super-agenda-groups ',(agile-gtd-priority-groups))))))
    ("rt" "Tangling TODOs"
     ((org-ql-block '(agile-gtd-tangling)
                    ((org-ql-block-header "Tangling TODOs")
                     (org-super-agenda-header-separator "")
                     (org-super-agenda-groups ',(agile-gtd-priority-groups))))))
    ("rS" "SOMEDAY"
     ((org-ql-block `(and (todo ,(agile-gtd--project-keyword))
                          (or (and (priority <= (char-to-string ,(agile-gtd--current-backlog-priority-threshold)))
                                   (not (ancestors (priority > (char-to-string ,(agile-gtd--current-backlog-priority-threshold)))))
                                   (not (children (priority > (char-to-string ,(agile-gtd--current-backlog-priority-threshold))))))
                              (tags ,agile-gtd-someday-tag)
                              (children (and (todo ,@(agile-gtd--action-keywords))
                                             (tags ,agile-gtd-someday-tag))))
                          (not (scheduled))
                          (not (habit))
                          (not (deadline)))
                    ((org-ql-block-header "Projects")
                     (org-super-agenda-header-separator "")
                     (org-super-agenda-groups ',(list (list :tag agile-gtd-someday-tag :order 10)
                                                      '(:auto-priority)))))))
    ("p" . "Private")
    ("pp" "Private Agenda Today"
     (,(agile-gtd--agenda-day (list (concat "-" agile-gtd-work-tag)))
      (org-ql-block ,(agile-gtd-agenda-query-stuck-projects '(agile-gtd-private))
                    ((org-ql-block-header "Stuck Projects")
                     (org-super-agenda-header-separator "")))
      (org-ql-block ,(agile-gtd-agenda-query-next-actions '(agile-gtd-private))
                    ((org-ql-block-header "Next Actions")
                     (org-super-agenda-groups ',(agile-gtd-ancestor-priority-groups))))))
    ("pb" "Private Backlog"
     ((org-ql-block ,(agile-gtd-agenda-query-backlog '(agile-gtd-private))
                    ((org-ql-block-header "Backlog")
                     (org-super-agenda-groups ',(agile-gtd-ancestor-priority-groups))
                     (org-dim-blocked-tasks t)))))
    ("ps" "Private Stuck Projects"
     ((org-ql-block ,(agile-gtd-agenda-query-stuck-projects '(agile-gtd-private))
                    ((org-ql-block-header "Stuck Projects")
                     (org-super-agenda-header-separator "")
                     (org-super-agenda-groups ,(agile-gtd-ancestor-priority-groups))))))
    ("w" . "Work")
    ("ww" "Work Agenda Today"
     (,(agile-gtd--agenda-day (list (concat "+" agile-gtd-work-tag)))
      (org-ql-block ,(agile-gtd-agenda-query-stuck-projects '(agile-gtd-work))
                    ((org-ql-block-header "Stuck Projects")
                     (org-super-agenda-header-separator "")))
      (org-ql-block ,(agile-gtd-agenda-query-next-actions '(agile-gtd-work))
                    ((org-ql-block-header "Next Actions")
                     (org-super-agenda-groups ,(agile-gtd-ancestor-priority-groups))))))
    ("wb" "Work Backlog"
     ((org-ql-block ,(agile-gtd-agenda-query-backlog '(agile-gtd-work))
                    ((org-ql-block-header "Backlog")
                     (org-super-agenda-groups ,(agile-gtd-ancestor-priority-groups))
                     (org-dim-blocked-tasks t)))))
    ("ws" "Work Stuck Projects"
     ((org-ql-block ,(agile-gtd-agenda-query-stuck-projects '(agile-gtd-work))
                    ((org-ql-block-header "Stuck Projects")
                     (org-super-agenda-header-separator "")
                     (org-super-agenda-groups ,(agile-gtd-ancestor-priority-groups))))))
    ,@(agile-gtd--customer-agenda-commands)))

(defun agile-gtd--agenda-someday-p ()
  "Return non-nil when the current agenda item is tagged as someday."
  (-find (-partial #'string= agile-gtd-someday-tag)
         (org-get-at-bol 'tags)))

(defun agile-gtd-agenda-set-someday (&optional do-schedule)
  "Mark the current agenda entry as SOMEDAY.

With prefix argument DO-SCHEDULE, schedule it as a tickler."
  (interactive "P")
  (org-agenda-set-tags agile-gtd-someday-tag 'on)
  (ignore-error user-error
    (org-agenda-priority 'remove))
  (org-agenda-deadline '(4))
  (org-agenda-schedule (unless do-schedule '(4))))

(defun agile-gtd-agenda-set-tickler ()
  "Mark the current agenda entry as a tickler."
  (interactive)
  (agile-gtd-agenda-set-someday '(4)))

(defun agile-gtd-agenda-remove-someday ()
  "Remove SOMEDAY and scheduling from the current agenda item."
  (interactive)
  (unless (agile-gtd--agenda-someday-p)
    (error "Element has no %s tag" agile-gtd-someday-tag))
  (org-agenda-set-tags agile-gtd-someday-tag 'off)
  (ignore-error user-error
    (org-agenda-priority 'remove))
  (org-agenda-deadline '(4))
  (org-agenda-schedule '(4)))

(defun agile-gtd-agenda-toggle-someday (&optional do-schedule)
  "Toggle SOMEDAY status for the current agenda item.

With prefix argument DO-SCHEDULE, create a tickler."
  (interactive "P")
  (if (agile-gtd--agenda-someday-p)
      (agile-gtd-agenda-remove-someday)
    (agile-gtd-agenda-set-someday (when do-schedule '(4)))))

(defun agile-gtd-agenda-toggle-tickler ()
  "Toggle SOMEDAY and ask for a tickler schedule."
  (interactive)
  (agile-gtd-agenda-toggle-someday '(4)))

(defun agile-gtd-agenda-show-priorities (&optional priority)
  "Show agenda items up to PRIORITY."
  (interactive "P")
  (let ((new-priority
         (cond ((equal priority '(4))
                (max agile-gtd-priority-highest (1- agile-gtd-priority-default)))
               (priority)
               (t (upcase (read-char (format "Show up to priority (%c-%c): "
                                             org-priority-highest
                                             org-priority-lowest)))))))
    (unless (agile-gtd--priority-in-range-p new-priority)
      (user-error "Priority must be between org-priority-highest and org-priority-lowest"))
    (setq agile-gtd-max-priority-group new-priority)
    (agile-gtd-refresh)
    (message "Showing up to priority %c" new-priority)
    (org-agenda-redo-all)))

(defun agile-gtd-agenda-reset-show-priorities ()
  "Reset the agenda priority filter."
  (interactive)
  (setq agile-gtd-max-priority-group nil)
  (agile-gtd-refresh)
  (org-agenda-redo-all))

(defun agile-gtd-agenda-show-more-priorities ()
  "Expand the agenda to include lower-priority items."
  (interactive)
  (setq agile-gtd-max-priority-group
        (min (1+ (agile-gtd--current-max-priority-group))
             agile-gtd-priority-lowest))
  (agile-gtd-refresh)
  (org-agenda-redo-all))

(defun agile-gtd-agenda-show-less-priorities ()
  "Restrict the agenda to higher-priority items."
  (interactive)
  (setq agile-gtd-max-priority-group
        (max (1- (agile-gtd--current-max-priority-group))
             agile-gtd-priority-highest))
  (agile-gtd-refresh)
  (org-agenda-redo-all))

(org-ql-defpred agile-gtd-tickler ()
  "Match entries in the tickler."
  :normalizers ((`(,predicate-names)
                 (rec `(and (todo)
                            (tags-local ,agile-gtd-someday-tag)
                            (scheduled))))))

(org-ql-defpred agile-gtd-tickler-proj ()
  "Match projects in the tickler and pure tickler subtrees."
  :normalizers ((`(,predicate-names)
                 (rec `(and (todo ,(agile-gtd--project-keyword))
                            (or (agile-gtd-tickler)
                                (and (children (agile-gtd-tickler))
                                     (not (children (and (todo ,@(agile-gtd--action-keywords))
                                                         (not (agile-gtd-tickler))))))))))))

(org-ql-defpred agile-gtd-work ()
  "Match work related entries."
  :normalizers ((`(,predicate-names)
                 (rec `(tags ,agile-gtd-work-tag)))))

(org-ql-defpred agile-gtd-private ()
  "Match private entries."
  :normalizers ((`(,predicate-names)
                 (rec `(not (tags ,agile-gtd-work-tag))))))

(org-ql-defpred (agile-gtd-stuck-proj agile-gtd-stuck) ()
  "Match stuck projects."
  :normalizers ((`(,predicate-names)
                 (rec `(and (todo ,(agile-gtd--project-keyword))
                            (not (tags ,agile-gtd-someday-tag))
                            (not (children (todo ,@(agile-gtd--action-keywords))))
                            (not (agile-gtd-tickler-proj)))))))

(org-ql-defpred agile-gtd-standalone-next ()
  "Match standalone NEXT and WAIT items."
  :normalizers ((`(,predicate-names)
                 (rec `(and (todo ,@(agile-gtd--action-keywords))
                            (not (ancestors (or (todo ,(agile-gtd--project-keyword))
                                                (done)))))))))

(org-ql-defpred agile-gtd-tangling ()
  "Match actions whose ancestors are done."
  :normalizers ((`(,predicate-names)
                 (rec '(and (todo)
                            (ancestors (done)))))))

(org-ql-defpred agile-gtd-someday ()
  "Match SOMEDAY items excluding ticklers."
  :normalizers ((`(,predicate-names)
                 (rec `(and (tags ,agile-gtd-someday-tag)
                            (not (agile-gtd-tickler)))))))

(org-ql-defpred agile-gtd-habit ()
  "Match habits by tag or style."
  :normalizers ((`(,predicate-names)
                 (rec `(or (tags ,agile-gtd-habit-tag)
                           (habit))))))

(org-ql-defpred agile-gtd-deadline-prio (op priority)
  "Match entries whose deadline-based priority satisfies OP relative to PRIORITY.
Example: (agile-gtd-deadline-prio <= ?C) matches items with deadline within 7 days."
  :body
  (let* ((element (org-element-at-point))
         (dl (org-element-property :deadline element)))
    (when dl
      (let* ((dl-days (- (time-to-days (org-timestamp-to-time dl))
                         (time-to-days (current-time))))
             (dl-rank (agile-gtd--deadline-rank dl-days))
             (prio-rank (agile-gtd--prio-rank priority)))
        (when prio-rank
          (funcall op dl-rank prio-rank))))))

(org-ql-defpred agile-gtd-parent-prio (op priority)
  "Match entries whose direct parent priority satisfies OP relative to PRIORITY.
Example: (agile-gtd-parent-prio <= ?C) matches items with parent priority A, B or C."
  :body
  (let* ((par-prio (agile-gtd--direct-parent-priority))
         (par-rank (agile-gtd--prio-rank par-prio))
         (prio-rank (agile-gtd--prio-rank priority)))
    (when (and par-rank prio-rank)
      (funcall op par-rank prio-rank))))

(defun agile-gtd--item-rank ()
  "Return the virtual priority rank for the Org item at point."
  (let* ((element (org-element-at-point))
         (prio (org-element-property :priority element))
         (parent-prio (agile-gtd--direct-parent-priority))
         (dl (org-element-property :deadline element))
         (dl-delta (when dl
                     (- (time-to-days (org-timestamp-to-time dl))
                        (time-to-days (current-time))))))
    (agile-gtd--backlog-rank prio parent-prio dl-delta)))

(defun agile-gtd--apply-priorities ()
  "Apply Agile GTD priority settings."
  (setq org-priority-highest agile-gtd-priority-highest
        org-priority-default agile-gtd-priority-default
        org-priority-lowest agile-gtd-priority-lowest
        org-priority-faces (agile-gtd--priority-faces)))

(defun agile-gtd--apply-org-modern-visuals ()
  "Apply Agile GTD org-modern visuals."
  (when agile-gtd-enable-org-modern-visuals
    (with-eval-after-load 'org-modern
      (setq org-modern-priority (agile-gtd--priority-symbols)))))

(defun agile-gtd--apply-todo-keywords ()
  "Apply Agile GTD TODO keywords and faces."
  (setq org-todo-keywords agile-gtd-todo-keywords
        org-todo-repeat-to-state agile-gtd-todo-repeat-to-state
        org-todo-keyword-faces
        '(("[-]" . agile-gtd-todo-active)
          ("NEXT" . agile-gtd-todo-next)
          ("WAIT" . agile-gtd-todo-onhold)
          ("IDEA" . agile-gtd-todo-idea)
          ("PROJ" . agile-gtd-todo-project)
          ("EPIC" . agile-gtd-todo-epic)
          ("KILL" . agile-gtd-todo-cancel))))

(defun agile-gtd--apply-tags ()
  "Apply Agile GTD workflow tags and customer tags."
  (let* ((workflow-tags (agile-gtd--workflow-tag-alist))
         (customer-tag-names (mapcar #'agile-gtd--customer-tag agile-gtd-customers))
         (managed-names (append (agile-gtd--workflow-tag-names) customer-tag-names))
         (current-tags (agile-gtd--delete-sublist workflow-tags org-tag-alist)))
    (setq org-tag-alist
          (append
           (cl-remove-if (lambda (entry)
                           (and (consp entry)
                                (stringp (car entry))
                                (member (car entry) managed-names)))
                         current-tags)
           workflow-tags))
    ;; Customer tags
    (dolist (customer agile-gtd-customers)
      (let ((tag (agile-gtd--customer-tag customer))
            (key (agile-gtd--customer-key customer)))
        (when (and tag key)
          (cl-pushnew (cons tag key) org-tag-alist
                      :test (lambda (a b) (equal (car a) (car b)))))))))

(defun agile-gtd--apply-refile-targets ()
  "Apply Agile GTD refile target settings."
  (when agile-gtd-enable-refile-targets
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 4)
                               (agile-gtd--someday-files :maxlevel . 4))
          org-refile-use-outline-path 'buffer-name
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm)))

(defun agile-gtd--apply-agenda-files ()
  "Apply Agile GTD agenda file settings."
  (when agile-gtd-enable-agenda-files
    (setq org-agenda-diary-file (agile-gtd--expand-org-path agile-gtd-diary-file)
          org-agenda-files (agile-gtd--agenda-files))))

(defun agile-gtd--apply-capture-templates ()
  "Apply Agile GTD capture templates."
  (setq org-capture-templates
        (agile-gtd--replace-by-key org-capture-templates
                                   (agile-gtd--capture-templates))))

(defun agile-gtd--apply-agenda-commands ()
  "Apply Agile GTD agenda commands and groups."
  (setq org-stuck-projects (agile-gtd--stuck-projects-setting)
        org-agenda-custom-commands
        (agile-gtd--replace-by-key org-agenda-custom-commands
                                   (agile-gtd--agenda-custom-commands)))
  (org-super-agenda-mode 1)
  (setq org-super-agenda-header-separator "\n"))

(defun agile-gtd-refresh ()
  "Refresh all derived Agile GTD configuration."
  (interactive)
  (agile-gtd--validate-configuration)
  (agile-gtd--apply-priorities)
  (agile-gtd--apply-org-modern-visuals)
  (agile-gtd--apply-todo-keywords)
  (agile-gtd--apply-tags)
  (agile-gtd--apply-agenda-files)
  (agile-gtd--apply-refile-targets)
  (agile-gtd--apply-capture-templates)
  (agile-gtd--apply-agenda-commands))

(defun agile-gtd-enable ()
  "Enable Agile GTD for the current Org configuration."
  (interactive)
  (agile-gtd-refresh))

(provide 'agile-gtd)

;;; agile-gtd.el ends here
