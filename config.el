(setq user-full-name "Stefan Lendl"
      user-mail-address "ste.lendl@gmail.com")

(setq auth-sources '("~/.config/authinfo/authinfo.gpg"))
      ;; auth-source-cache-expiry nil) ; default is 7200 (2h)

(defun get-auth-info (host user &optional port)
  (let ((info (nth 0 (auth-source-search
                      :host host
                      :user user
                      :port port
                      :require '(:user :secret)))))
    (if info
        (let ((secret (plist-get info :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      nil)))

(when (equal (window-system) nil)
  (and
   (bind-key "C-<down>" #'+org/insert-item-below)
   (setq doom-theme 'doom-solarized-dark)
   (setq doom-font (font-spec :family "Source Code Pro" :size 20))))

(toggle-frame-maximized)

;; (setq doom-font (font-spec :family "Fira Code" :size 13)
;;       doom-variable-pitch-font (font-spec :family "Fira Code")
;;       doom-unicode-font (font-spec :family "DejaVu Sans Mono")
;;       doom-big-font (font-spec :family "Fira Code Medium" :size 20))
(setq doom-font (font-spec :family "JetBrains Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "JetBrains Mono" :size 20))

;; (setq doom-theme 'zaiste)
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-molokai)
;; (setq doom-theme 'doom-solarized-dark)

;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; (use-package! fira-code-mode
;;   :after prog-mode
;;   :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" "*" "+" ":")) ;; List of ligatures to turn off
;;   )

(setq! display-line-numbers-type t)
(setq! which-key-idle-delay 0.3)

(set-popup-rule! "^CAPTURE" :side 'bottom :size 0.40 :select t :ttl nil)

(after! org-ql
  (set-popup-rule!
    "^\\*Org QL View" :side 'left :size 0.50 :select t :quit nil
    ))

(after! evil-snipe
  (setq evil-snipe-scope 'buffer)
  )

;; (global-auto-revert-mode 1)
(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      inhibit-compacting-font-caches t)
;; (whitespace-mode -1)

(setq org-directory "~/.org/")

(after! org
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))
        org-ellipsis " ▼"
        ))

(after! org
  (setq org-id-link-to-org-use-id t
        org-id-locations-file "~/.emacs.d/.local/.org-id-locations"
        org-id-track-globally t))

(after! org
  ;; (async-start
   (org-id-update-id-locations)
   ;; 'ignore)
   )

(after! org
  (add-hook 'auto-save-hook 'org-save-all-org-buffers 10)
  (add-hook 'auto-save-hook 'org-id-update-id-locations 20))

(after! org-roam
  (add-hook 'auto-save-hook 'org-roam-build-cache 40))

;; (after! org
  (setq org-startup-indented 'indent
        org-startup-folded 'fold
        org-startup-with-inline-images t
        )
;)
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'turn-off-auto-fill)

(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (ignore-errors
    (base64-decode-string link)))

;; (bind-key "<f6>" #'link-hint-copy-link)
(map! :after org
      :map org-mode-map
      :leader
      :prefix ("n" . "notes")
      :desc "Revert all org buffers" "R" #'org-revert-all-org-buffers
      :desc "Revert all org buffers" "R" #'org-revert-all-org-buffers
      )

;; Die sind eigentlich nicht org spezifisch
      ;; :desc "Outline" "o" #'counsel-outline
      ;; :desc "Counsel ripgrep" "d" #'counsel-rg
      ;; :desc "Swiper All" "@" #'swiper-all

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Revert all org buffers" "R" #'org-revert-all-org-buffers

      :prefix ("s" . "Tree/Subtree")
      :desc "Rifle Org Directory" "/" #'helm-org-rifle-org-directory
      :desc "Rifle Buffer" "B" #'helm-org-rifle-current-buffer
      :desc "Rifle Agenda Files" "A" #'helm-org-rifle-agenda-files
      :desc "Rifle Project Files" "#" #'helm-org-rifle-project-files
      :desc "Rifle Other Project(s)" "$" #'helm-org-rifle-other-files
      :desc "Match sparse tree" "M" #'org-match-sparse-tree

      :prefix ("l" . "links")
      "o" #'org-open-at-point
      "g" #'eos/org-add-ids-to-headlines-in-file

      :prefix ("r" . "refile")
      :desc "Refile to reference" "R" #'stfl/refile-to-roam
      :desc "create org-roam note from headline" "h" #'org-roam-create-note-from-headline
      )

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      :desc "Filter" "f" #'org-agenda-filter
      :desc "Follow" "F" #'org-agenda-follow-mode
      ;; :desc "Priority" "p" #'org-agenda-priority
      ;; :prefix ("s" . "search and set")
      :prefix ("p" . "priorities")
      :desc "Prioity" "p" #'org-agenda-priority
      :desc "Prioity up" "u" #'org-agenda-priority-up
      :desc "Prioity down" "d" #'org-agenda-priority-down
      :desc "Prioity tree" "P" #'org-agenda-priority-tree
      :desc "Prioity tree up" "U" #'org-agenda-priority-tree-up
      :desc "Prioity tree down" "D" #'org-agenda-priority-tree-down
      )

(map! ;;:after org-agenda
      :map org-agenda-mode-map
      :desc "Prioity tree up" "C-S-k" #'org-agenda-priority-tree-up
      :desc "Prioity tree down" "C-S-j" #'org-agenda-priority-tree-down
      )

;; (defun zyro/rifle-roam ()
;;   "Rifle through your ROAM directory"
;;   (interactive)
;;   (helm-org-rifle-directories org-roam-directory))

;; (map! :after org
;;       :map org-mode-map
;;       :leader
;;       :prefix ("n" . "notes")
;;       :desc "Rifle ROAM Notes" "!" #'zyro/rifle-roam)

(after! org (setq org-agenda-diary-file "~/.org/diary.org"
                  ;; org-agenda-dim-blocked-tasks t
                  org-agenda-dim-blocked-tasks 'invisible
                  org-agenda-use-time-grid t
                  ;; org-agenda-hide-tags-regexp "\\w+"
                  org-agenda-compact-blocks nil
                  org-agenda-block-separator ""
                  org-agenda-skip-scheduled-if-done t
                  org-agenda-skip-unavailable-files t
                  org-agenda-skip-deadline-if-done t
                  org-agenda-window-setup 'current-window
                  org-agenda-start-on-weekday nil
                  org-agenda-span 'day
                  org-agenda-start-day "-0d"
                  org-deadline-warning-days 7
                  org-enforce-todo-checkbox-dependencies nil
                  org-enforce-todo-dependencies nil
                  org-habit-show-habits t))

(after! org (setq org-agenda-files '("~/.org/gtd/inbox.org"
                                     ;; "~/.org/gtd/someday.org"
                                     "~/.org/gtd/tickler.org"
                                     "~/.org/calendar.org"
                                     "~/.org/gtd/projects.org"
                                     "~/.org/gtd/projects/")))
;; (append (file-expand-wildcards "~/.org/gtd/*.org")
;;         (file-expand-wildcards "~/.org/gtd/projects/*.org"))))

;; (after! org
;;   (setq org-agenda-files '("~/.org/gtd/inbox.org"
;;                            "~/.org/gtd/projects.org"
;;                            "~/.org/gtd/tickler.org"))

(after! org
  (setq! org-clock-continuously t))

(defun skip-all-siblings-but-first-next-action ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-next-action)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-next-action ()
  (string= "NEXT" (org-get-todo-state)))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(use-package! org-super-agenda
  :after org-agenda
  ;; :init
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap)) ;; don't break evil on org-super-agenda headings, see https://github.com/alphapapa/org-super-agenda/issues/50

  (setq org-super-agenda-groups
        '((:name "Today"
           :deadline past
           :deadline today
           :scheduled today
           :scheduled past)
          (:name "Next Actions" :todo "NEXT")
          (:name "Waiting for" :todo "WAIT")
          (:name "Projects"
           :and (:todo "PROJ"
                 :children ("NEXT"))
           :order 5)
          (:name "Waiting Projects"
           :and (:todo "PROJ"
                 :children ("WAIT"))
           :order 6)
          (:name "Stuck Projects"   ;; the rest but show before Projects
           :todo "PROJ"
           :order 4)))
  )

(after! org-ql)

(after! org (setq org-capture-templates
                  '(("!" "Quick Capture" plain (file "~/.org/gtd/inbox.org")
                     "* TODO %(read-string \"Task: \")\n:PROPERTIES:\n:CREATED: %U\n:END:")
                    ("p" "New Project" plain (file nm/org-capture-file-picker)
                     (file "~/.doom.d/templates/template-projects.org"))
                    ("n" "Note on headline" plain (function nm/org-end-of-headline)
                     "%?" :empty-lines-before 1 :empty-lines-after 1)
                    ("q" "quick note to file" entry (function nm/org-capture-weeklies)
                     "* %?" :empty-lines-before 1 :empty-lines-after 1)
                    ("P" "Protocol" plain (file "~/.org/gtd/inbox.org")
                     "* %^{Title}\nSource: [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
                     :empty-lines-after 1)
                    ("L" "Protocol Link" plain (file "~/.org/gtd/inbox.org")
                     "* [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
                     :empty-lines-after 1 )
                    )
                  ))

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
  (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )

(after! org
  (setq org-image-actual-width nil
        org-archive-location "~/.org/gtd/archive/%s::datetree"
        ))

(after! org-agenda (require 'org-habit))

(use-package! org-edna
  :after org
  ;; :config (org-edna-mode)
  )

(defun stfl/trigger-next-sibling-NEXT ()
  (interactive)
  (org-entry-put nil "TRIGGER" "next-sibling todo!(NEXT)"))

(defun stfl/blocker-previous-sibling ()
  (interactive)
  (org-entry-put nil "BLOCKER" "previous-sibling"))

(defun stfl/trigger-next-and-blocker-previous ()
  (interactive)
  (stfl/trigger-next-sibling-NEXT)
  (stfl/blocker-previous-sibling))

(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("d" . "date/dateline/dependencies")
      :desc "next-sibling NEXT" "n" 'stfl/trigger-next-sibling-NEXT
      :desc "trigger NEXT and block prev" "b" 'stfl/trigger-next-and-blocker-previous
      )

(custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-next '((t (:inherit (bold font-lock-keyword-face org-todo)))) "")
(custom-declare-face 'org-checkbox-statistics-todo '((t (:inherit (bold font-lock-constant-face org-todo)))) "")

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; Project with multiple task items.
           "NEXT(n!)"  ; Task is next to be worked on.
           "WAIT(w!/!)"  ; Something external is holding up this task
           "|"
           "DONE(d@)"  ; Task successfully completed
           "KILL(k@)")) ; Task was cancelled, aborted or is no longer applicable
        org-todo-keyword-faces
        '(("WAIT" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("TODO" . +org-todo-active)
          ("NEXT" . +org-todo-next)))
)

(after! org (setq org-indent-indentation-per-level 4))

;; (set-ligatures! 'org-mode
;;     :alist '(("TODO " . "")
;;              ("NEXT " . "")
;;              ("PROJ " . "")
;;              ("WAIT " . "")
;;              ("DONE " . "")
;;              ("KILL " . "")))

(set-ligatures! 'org-mode
    :alist '((":PROPERTIES:" . "⏍")
             (":properties:" . "⏍")
             (":LOGBOOK:" . "㏒")
             (":logbook:" . "㏒")
             ))

(after! org-superstar
  ;; Every non-TODO headline now have no bullet
  ;; (setq org-superstar-headline-bullets-list '("　"))
  (setq org-superstar-leading-bullet ?　)
  ;; Enable custom bullets for TODO items
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-todo-bullet-alist
        '(("TODO" "☐")
          ("NEXT" "➡")
          ("PROJ" "⎚")
          ("WAIT" "⏳")
          ("KILL" "✘")
          ("DONE" "✔")))
  (org-superstar-restart))

(after! org (setq org-log-state-notes-insert-after-drawers nil))

(after! org (setq org-log-into-drawer t
                  org-log-done 'time+note
                  org-log-repeat 'time
                  org-log-redeadline 'time
                  org-log-reschedule 'time
                  ))

(after! org (setq org-use-property-inheritance t ; We like to inherit properties from their parents
                  org-catch-invisible-edits 'error ; Catch invisible edits
                  org-track-ordered-property-with-tag t
                  org-hierarchical-todo-statistics nil
                  ))

;; (after! org
;;   (defun my/org-inherited-priority (s)
;;     (cond

;;      ;; Priority cookie in this heading
;;      ((string-match org-priority-regexp s)
;;       (* 1000 (- org-priority-lowest
;;                  (org-priority-to-value (match-string 2 s)))))

;;      ;; No priority cookie, but already at highest level
;;      ((not (org-up-heading-safe))
;;       (* 1000 (- org-priority-lowest org-priority-default)))

;;      ;; Look for the parent's priority
;;      (t
;;       (my/org-inherited-priority (org-get-heading)))))

;;   (setq org-priority-get-priority-function #'my/org-inherited-priority)
;;   )

(after! org
  (defun my/org-priority-up ()
    (org-priority 'up))

  (defun my/org-priority-down ()
    (org-priority 'down))

  (defun my/org-priority-up-region ()
    (interactive)
    (org-map-entries #'my/org-priority-up nil 'region)
    (setq deactivate-mark nil))

  (defun my/org-priority-down-region ()
    (interactive)
    (org-map-entries #'my/org-priority-down nil 'region)
    (setq deactivate-mark nil))
  )

(after! org-agenda

  (defun org-agenda-priority-tree-up (&optional force-direction)
    "Increase the priority of line at point, also in Org file."
    (interactive "P")
    (if (equal force-direction '(4))
        (org-priority-show)
      (unless org-priority-enable-commands
        (user-error "Priority commands are disabled"))
      (org-agenda-check-no-diary)
      (let* ((col (current-column))
         (marker (or (org-get-at-bol 'org-marker)
                 (org-agenda-error)))
         (hdmarker (org-get-at-bol 'org-hd-marker))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
        (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (org-map-entries '(org-priority 'up) nil 'tree)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
	  (org-move-to-column col)))))

  (defun org-agenda-priority-tree-down (&optional force-direction)
    "Decrease the priority of line at point, also in Org file."
    (interactive "P")
    (if (equal force-direction '(4))
        (org-priority-show)
      (unless org-priority-enable-commands
        (user-error "Priority commands are disabled"))
      (org-agenda-check-no-diary)
      (let* ((col (current-column))
         (marker (or (org-get-at-bol 'org-marker)
                 (org-agenda-error)))
         (hdmarker (org-get-at-bol 'org-hd-marker))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
        (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (org-map-entries '(org-priority 'down) nil 'tree)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
	  (org-move-to-column col)))))

  (defun org-agenda-priority-tree (&optional force-direction)
    "Set the priority of line at point, also in Org file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org file.
Called with a universal prefix arg, show the priority instead of setting it."
    (interactive "P")
    (if (equal force-direction '(4))
        (org-priority-show)
      (unless org-priority-enable-commands
        (user-error "Priority commands are disabled"))
      (org-agenda-check-no-diary)
      (let* ((col (current-column))
         (marker (or (org-get-at-bol 'org-marker)
                 (org-agenda-error)))
         (hdmarker (org-get-at-bol 'org-hd-marker))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
        (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (org-map-entries '(org-priority 'set) nil 'tree)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
	  (org-move-to-column col)))))
  )

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      :prefix("s")
      :desc "Prioity up region" "K" #'my/org-priority-up-region
      :desc "Prioity down region" "J" #'my/org-priority-donw-region
      )

(defun stfl/build-my-someday-files ()
  (file-expand-wildcards "~/.org/gtd/someday/*.org"))

(after! org
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 4)
                             ("~/.org/gtd/someday.org" :maxlevel . 4)
                             (stfl/build-my-someday-files :maxlevel . 4))
        org-refile-use-outline-path 'buffer-name
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))

(defun stfl/build-my-roam-files () (file-expand-wildcards "~/.org/roam/**/*.org"))

(defun stfl/refile-to-roam ()
  (interactive)
  (let ((org-refile-targets '((stfl/build-my-roam-files :maxlevel . 1))))
    (call-interactively 'org-refile)))

;; (defun stfl/refile-to-roam ()
;;   (interactive)
;;   (setq stfl/org-roam-files (append (file-expand-wildcards "~/.org/roam/**/*.org")))
;;   (let ((org-refile-targets '((stfl/org-roam-files :maxlevel . 4))))
;;     (call-interactively 'org-refile)))

;; ;; initial prompt should be the text of the tree
;; (defun stfl/refile-to-roam2 (&optional initial-prompt)
;;   (interactive)
;;   ;; (setq stfl/org-roam-files (append (file-expand-wildcards "~/.org/roam/**/*.org")))
;;   (let* ((completions (org-roam--get-title-path-completions))
;;          (title-with-tags (org-roam-completion--completing-read "File: " completions :initial-input initial-prompt))
;;          (res (cdr (assoc title-with-tags completions)))
;;          (file-path (plist-get res :path)))
;;     ;; if we have a file-path -> call org-refile
;;     (if file-path
;;         (type-of file-path)
;;       (let ((org-refile-targets (quote ((file-path :maxlevel . 4))))
;;         (call-interactively 'org-refile))
;;       ;; if we can't find a file call a org-roam-capture
;;       ;; TODO this does not actually refile the subtree
;;       (let ((org-roam-capture--info `((title . ,title-with-tags)
;;                                       (slug  . ,(funcall org-roam-title-to-slug-function title-with-tags))))
;;             (org-roam-capture--context 'title))
;;         (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
;;         (org-roam-capture--capture))
;;       )))

(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

;; (setq org-tags-column 0)
(setq org-tag-alist '((:startgrouptag)
                      ("Context" . nil)
                      (:grouptags)
                      ("@home" . ?h)
                      ("@office". ?o)
                      ("@sarah" . ?s)
                      (:endgrouptag)
                      (:startgrouptag)
                      ("@PC" . ?p)
                      (:grouptags)
                      ("@desktop")
                      ("@laptop")
                      (:endgrouptag)
                      (:startgrouptag)
                      ("Categories" . nil)
                      (:grouptags)
                      ("wohnung")
                      ("health")
                      ("bike")
                      ("friends")
                      ("emacs")
                      ("gtd")
                      ("shopping")
                      ("learning")
                      (:endgrouptag)
                      (:startgrouptag)
                      ("Process" . nil)
                      (:grouptags)
                      ("SOMEDAY" . ?S)
                      ("CANCELLED" . ?C)
                      ("HOLD" . ?H)
                      ("REFILE" . ?R)
                      ("WAITING" . ?W)
                      (:endgrouptag)
                      (:startgrouptag)
                      ("Areas" . nil)
                      (:grouptags)
                      ("pulswerk" . ?$)
                      ("#personal" . ?_)
                      ))

(use-package! org-edna
  :after org)
(add-hook 'org-mode-hook 'org-edna-mode)

(defun gtd/planning-trigger ()
  "Automatically schedule an entry when it becomes NEXT according to PLANNED property"
  (when (equal org-state "NEXT")
    (message "das war next")
    (setq planned (car (org-map-entries (lambda () (
      org-entry-get nil  "PLANNED")) "PLANNED<>\"\"" 'tree)))
    (if planned (
      (message "Geplant ist %s" planned)
      (org-entry-put nil "SCHEDULED" planned)
      (org-entry-delete nil "PLANNED")
  ) nil) ))

;; (add-hook 'org-after-todo-state-change-hook 'gtd/planning-trigger)

(use-package helm-org-rifle
  :after (helm org)
  :preface
  (autoload 'helm-org-rifle-wiki "helm-org-rifle")
  :config
  (add-to-list 'helm-org-rifle-actions '("Insert link" . helm-org-rifle--insert-link) t)
  (add-to-list 'helm-org-rifle-actions '("Store link" . helm-org-rifle--store-link) t)
  (defun helm-org-rifle--store-link (candidate &optional use-custom-id)
    "Store a link to CANDIDATE."
    (-let (((buffer . pos) candidate))
      (with-current-buffer buffer
        (org-with-wide-buffer
         (goto-char pos)
         (when (and use-custom-id
                    (not (org-entry-get nil "CUSTOM_ID")))
           (org-set-property "CUSTOM_ID"
                             (read-string (format "Set CUSTOM_ID for %s: "
                                                  (substring-no-properties
                                                   (org-format-outline-path
                                                    (org-get-outline-path t nil))))
                                          (helm-org-rifle--make-default-custom-id
                                           (nth 4 (org-heading-components))))))
         (call-interactively 'org-store-link)))))

  ;; (defun helm-org-rifle--narrow (candidate)
  ;;   "Go-to and then Narrow Selection"
  ;;   (helm-org-rifle-show-entry candidate)
  ;;   (org-narrow-to-subtree))

  (defun helm-org-rifle--store-link-with-custom-id (candidate)
    "Store a link to CANDIDATE with a custom ID.."
    (helm-org-rifle--store-link candidate 'use-custom-id))

  (defun helm-org-rifle--insert-link (candidate &optional use-custom-id)
    "Insert a link to CANDIDATE."
    (unless (derived-mode-p 'org-mode)
      (user-error "Cannot insert a link into a non-org-mode"))
    (let ((orig-marker (point-marker)))
      (helm-org-rifle--store-link candidate use-custom-id)
      (-let (((dest label) (pop org-stored-links)))
        (org-goto-marker-or-bmk orig-marker)
        (org-insert-link nil dest label)
        (message "Inserted a link to %s" dest))))

  (defun helm-org-rifle--make-default-custom-id (title)
    (downcase (replace-regexp-in-string "[[:space:]]" "-" title)))

  (defun helm-org-rifle--insert-link-with-custom-id (candidate)
    "Insert a link to CANDIDATE with a custom ID."
    (helm-org-rifle--insert-link candidate t))

  (helm-org-rifle-define-command
   "wiki" ()
   "Search in \"~/lib/notes/writing\" and `plain-org-wiki-directory' or create a new wiki entry"
   :sources `(,(helm-build-sync-source "Exact wiki entry"
                 :candidates (plain-org-wiki-files)
                 :action #'plain-org-wiki-find-file)
              ,@(--map (helm-org-rifle-get-source-for-file it) files)
              ,(helm-build-dummy-source "Wiki entry"
                 :action #'plain-org-wiki-find-file))
   :let ((files (let ((directories (list "~/lib/notes/writing"
                                         plain-org-wiki-directory
                                         "~/lib/notes")))
                  (-flatten (--map (f-files it
                                            (lambda (file)
                                              (s-matches? helm-org-rifle-directories-filename-regexp
                                                          (f-filename file))))
                                   directories))))
         (helm-candidate-separator " ")
         (helm-cleanup-hook (lambda ()
                              ;; Close new buffers if enabled
                              (when helm-org-rifle-close-unopened-file-buffers
                                (if (= 0 helm-exit-status)
                                    ;; Candidate selected; close other new buffers
                                    (let ((candidate-source (helm-attr 'name (helm-get-current-source))))
                                      (dolist (source helm-sources)
                                        (unless (or (equal (helm-attr 'name source)
                                                           candidate-source)
                                                    (not (helm-attr 'new-buffer source)))
                                          (kill-buffer (helm-attr 'buffer source)))))
                                  ;; No candidates; close all new buffers
                                  (dolist (source helm-sources)
                                    (when (helm-attr 'new-buffer source)
                                      (kill-buffer (helm-attr 'buffer source))))))))))
  :general
  (:keymaps 'org-mode-map
   "M-s r" #'helm-org-rifle-current-buffer)
  :custom
  (helm-org-rifle-directories-recursive t)
  (helm-org-rifle-show-path t)
  (helm-org-rifle-test-against-path t))

(provide 'setup-helm-org-rifle)

(setq! org-roam-tag-sources '(prop last-directory)
       org-roam-db-location "~/.emacs.d/roam.db"
       org-roam-directory "~/.org/")

(setq! org-roam-file-exclude-regexp "*/.stversions/*")
;; (add-to-list 'safe-local-variable-values '(org-roam-directory . "."))

(setq org-roam-dailies-capture-templates
   '(("d" "daily" plain (function org-roam-capture--get-point) ""
      :immediate-finish t
      :file-name "roam/journal/%<%Y-%m-%d-%a>"
      :head "#+TITLE: %<%Y-%m-%d %a>\n#+STARTUP: content\n\n")))

(setq org-roam-capture-templates
        '(("f" "fleeting" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "roam/fleeting/${slug}"
           :head "#+title: ${title}\n#+roam_tags: %^{tags}\n\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "roam/private/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("c" "coding" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "roam/coding/${slug}"
           :head "#+title: ${title}\n#+roam_tags: %^{tags}\n\n"
           :unnarrowed t)
           ))

(after! org-roam (org-roam-db-build-cache))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8070
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll nil
        org-roam-server-network-arrows 'from
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(defun my/org-roam--backlinks-list-with-content (file)
  (with-temp-buffer
    (if-let* ((backlinks (org-roam--get-backlinks file))
              (grouped-backlinks (--group-by (nth 0 it) backlinks)))
        (progn
          (insert (format "\n\n* %d Backlinks\n"
                          (length backlinks)))
          (dolist (group grouped-backlinks)
            (let ((file-from (car group))
                  (bls (cdr group)))
              (insert (format "** [[file:%s][%s]]\n"
                              file-from
                              (org-roam--get-title-or-slug file-from)))
              (dolist (backlink bls)
                (pcase-let ((`(,file-from _ ,props) backlink))
                  (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
                  (insert "\n\n")))))))
    (buffer-string)))

(defun my/org-export-preprocessor (backend)
  (let ((links (my/org-roam--backlinks-list-with-content (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n") links)))))

(add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

(use-package! org-jira
  :after org
  :init
  (setq
   org-jira-working-dir "~/.org/jira/"
        jiralib-url "https://pulswerk.atlassian.net"))

(use-package! ejira
  :after org
  :init
  (setq jiralib2-url              "https://pulswerk.atlassian.net"
        jiralib2-auth             'token
        jiralib2-user-login-name  "lendl@pulswerk.at"
        jiralib2-token            (get-auth-info "pulswerk.atlassian.net" "lendl@pulswerk.at")

        ejira-org-directory       "~/.org/ejira"
        ejira-projects            '("MD")

        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?A)
                                    ("Medium"  . ?B)
                                    ("Low"     . ?C)
                                    ("Lowest"  . ?C))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Testing" . 3)
                                    ("Done"        . 4)))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  ;; (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda))

(use-package! org-gcal
  :commands (org-gcal-sync
             org-gcal-fetch
             org-gcal-post-at-point
             org-gcal-delete-at-point)
  ;; :init
  ;; (defvar org-gcal-dir (concat doom-cache-dir "org-gcal/"))
  ;; (defvar org-gcal-token-file (concat org-gcal-dir "token.gpg"))
  :config
  ;; hack to avoid the deferred.el error
  (defun org-gcal--notify (title mes)
    (message "org-gcal::%s - %s" title mes))
  (setq org-gcal-client-id (get-auth-info "org-gcal-client-id" "ste.lendl@gmail.com")
        org-gcal-client-secret (get-auth-info "org-gcal-client-secret" "ste.lendl@gmail.com")
        org-gcal-fetch-file-alist '(("ste.lendl@gmail.com" .  "~/.org/calendar.org"))))

(use-package! ivy-omni-org
  ;; :after org
  :commands (ivy-omni-org ivy-omni-org-bookmarks)
  )

;; (after! org
;;   (set-company-backend! 'org-mode 'company-capf '(company-yasnippet company-org-roam company-elisp))
;;   (setq company-idle-delay 0.25))

(use-package! define-word
  :after org
  :config
  (map! :after org
        :map org-mode-map
        :leader
        :desc "Define word at point" "@" #'define-word-at-point))

(setq deft-use-projectile-projects t)
(defun zyro/deft-update-directory ()
  "Updates deft directory to current projectile's project root folder and updates the deft buffer."
  (interactive)
  (if (projectile-project-p)
      (setq deft-directory (expand-file-name (doom-project-root)))))
(when deft-use-projectile-projects
  (add-hook 'projectile-after-switch-project-hook 'zyro/deft-update-directory)
  (add-hook 'projectile-after-switch-project-hook 'deft-refresh))

(load! "my-deft-title.el")
(use-package deft
  :commands (deft deft-open-file deft-new-file-named)
  :config
  (setq deft-directory "~/.org/"
        deft-auto-save-interval 0
        deft-recursive t
        deft-current-sort-method 'title
        deft-extensions '("md" "txt" "org")
        deft-use-filter-string-for-filename t
        deft-use-filename-as-title nil
        deft-markdown-mode-title-level 1
        deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\)$\\|\\.stversions"
        deft-file-naming-rules '((nospace . "-"))))
(require 'my-deft-title)
(advice-add 'deft-parse-title :around #'my-deft/parse-title-with-directory-prepended)

(setq org-pandoc-options '((standalone . t) (self-contained . t)))

(after! projectile
  ;; (setq projectile-project-search-path
  ;;       (cddr (directory-files "/work" t))) ;;add all dirs inside ~/work -> https://github.com/bbatsov/projectile/issues/1500
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  )

;; (use-package! edbi
  ;; :command "edbi:open-db-viewer"
  ;; )

(after! ediff
  (setq ediff-diff-options "--text"
        ediff-diff3-options "--text"))

(use-package! origami)

(map! :after '(org-agenda origami)
      :map org-agenda-mode-map
      :desc "" "TAB" #'origami-toggle-node
      ;; :desc "" "" #'org-agenda-priority-tree-down
      )

(load! "org-customs.el")
(load! "org-helpers.el")
(load! "org-helpers-nm.el")

;; (setq org-tasks-properties-metadata (list "SOURCE"))
;; (map! :after org
;;       :map org-mode-map
;;       :localleader
;;       :prefix ("j" . "nicks functions")
;;       :desc "Clarify properties" "c" #'nm/org-clarify-metadata)

;; (bind-key "<f7>" #'nm/org-capture-to-file)

;; (add-hook 'before-save-hook #'nm/org-assign-tasks-proj)

(use-package! lsp-treemacs
  :after lsp-mode  ;; and treemacs
  :config (lsp-treemacs-sync-mode 1)
  )

;; improve performance of lsp-mode https://emacs-lsp.github.io/lsp-mode/page/performance/
(after! lsp-mode
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  ;; (setq lsp-log-io t)
  )

(map! (:after lsp
       :map lsp-mode-map
       :leader
       :prefix ("c" . "+code")
       :desc "Diagnostic for Workspace" "X" #'lsp-treemacs-errors-list))

(after! (lsp-mode php-mode)
  (setq lsp-intelephense-files-associations '["*.php" "*.phtml" "*.inc"])
  (setq lsp-intelephense-files-exclude '["**update.php**" "**/js/**" "**/fonts/**" "**/gui/**" "**/upload/**"
                                         "**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"])
  (setq lsp-intelephense-licence-key (get-auth-info "intelephense" "sutter"))
  (setq lsp-intelephense-trace-server "verbose")
  (setq lsp-intelephense-multi-root nil)
  ;; (setq lsp-intelephense-clear-cache t)
  (setq lsp-auto-guess-root nil)
  (setq lsp-idle-delay 0.5)
  )


;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02
;; (dap-php-setup)
;; (dap-register-debug-template
;;   "Php Remote Debug"
;;   (list :type "php"
;;         :cwd nil
;;         :request "launch"
;;         :name "Php Remote Debug"
;;         :args '("--server=4711")
;;         :pathMappings (ht ("/var/www/html" (projectile-project-root (buffer-file-name))))
;;         :sourceMaps t))

(add-to-list 'auto-mode-alist '("\\.mq[45h]\\'" . cpp-mode))

(use-package! gitlab-ci-mode
  :mode ".gitlab-ci.yml"
  )

(use-package! gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :init
  (gitlab-ci-mode-flycheck-enable))

(use-package! ztree)

(after! forge (setq forge-topic-list-columns
                    '(("#" 5 t (:right-align t) number nil)
                      ("Title" 60 t nil title  nil)
                      ("State" 6 t nil state nil)
                      ("Marks" 8 t nil marks nil)
                      ("Labels" 8 t nil labels nil)
                      ("Assignees" 10 t nil assignees nil)
                      ("Updated" 10 t nill updated nil))))

(use-package! with-editor
  :after magit
  :config
  (define-key (current-global-map)
    [remap async-shell-command] 'with-editor-async-shell-command)
  (define-key (current-global-map)
    [remap shell-command] 'with-editor-shell-command)

  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)

  (add-hook 'shell-mode-hook 'with-editor-export-git-editor)
)

(after! todoist (setq todoist-token (get-auth-info "todoist" "stfl")))

(use-package! ejira
  ;; :after org
  :init
  (setq jiralib2-url              "https://pulswerk.atlassian.net"
        jiralib2-auth             'token
        jiralib2-user-login-name  "lendl@pulswerk.at"
        jiralib2-token            (get-auth-info "pulswerk.atlassian.net" "lendl@pulswerk.at")

        ejira-org-directory       "~/.org/ejira"
        ejira-projects            '("MD")

        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Testing" . 3)
                                    ("Done"        . 4)))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda))

(use-package! org-jira
  :after org
  :init
  (setq
   org-jira-working-dir "~/.org/jira/"
        jiralib-url "https://pulswerk.atlassian.net"))

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(defun nm/org-id-prompt-id ()
  "Prompt for the id during completion of id: link."
  (let ((dest (org-refile-get-location))
        (name nil)
        (id nil))
    (save-excursion
      (find-file (cadr dest))
      (goto-char (nth 3 dest))
      (setq id (org-id-get (point) t)
            name (org-get-heading t t t t)))
    (org-insert-link nil (concat "id:" id) name)))

(after! org
  (org-link-set-parameters "id" :complete #'nm/org-id-prompt-id))
