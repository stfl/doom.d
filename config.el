(setq user-full-name "Stefan Lendl"
      user-mail-address "ste.lendl@gmail.com")

(setq auth-sources '("~/.config/authinfo/authinfo.gpg")
       ;; auth-source-cache-expiry nil ; default is 7200 (2h)
      )
;; (require 'auth-source)
;; (auth-source-forget-all-cached)  ;; forget all cached entries to force loading entries at start

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
    "^\\*Org QL View" :side 'left :size 0.40 :select t :quit nil
    ))

(custom-set-faces!
 '(org-date :foreground "dark goldenrod" :height 0.85)
 '(org-document-title :foreground "#c678dd" :weight bold :height 1.8)
 '(org-drawer :foreground "dark gray" :height 0.8)
 '(outline-1 :height 1.5)
 '(outline-2 :height 1.25)
 '(outline-3 :height 1.15)
 '(org-property-value :height 0.85)
 '(org-special-keyword :foreground "#83898d" :height 0.8)
 '(org-tag :foreground "#83898d" :weight light :height 0.7)
 )

;; (custom-set-faces!
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it byrhand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-date ((t (:foreground "dark goldenrod" :height 0.85))))
;;  '(org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.8))))
;;  '(org-drawer ((t (:foreground "dark gray" :height 0.8))))
;;  '(org-level-1 ((t (:inherit outline-1 :extend t :height 1.5))))
;;  '(org-level-2 ((t (:inherit outline-2 :extend t :height 1.25))))
;;  '(org-level-3 ((t (:inherit outline-3 :extend t :height 1.15))))
;;  '(org-level-4 ((t (:inherit outline-4 :extend t :height 1.0))))
;;  '(org-level-5 ((t (:inherit outline-5 :extend t))))
;;  '(org-level-6 ((t (:inherit outline-6 :extend t))))
;;  '(org-level-7 ((t (:inherit outline-7 :extend t))))
;;  '(org-level-8 ((t (:inherit outline-8 :extend t))))
;;  '(org-property-value ((t (:height 0.85))) t)
;;  '(org-special-keyword ((t (:foreground "#83898d" :height 0.8))))
;;  '(org-tag ((t (:foreground "#83898d" :weight light :height 0.7))))
;;  )
 ;; '(fixed-pitch ((t (:font #<font-spec nil nil JetBrains\ Mono nil nil nil nil nil 13 nil nil nil nil>))))
 ;; '(variable-pitch ((t (:font #<font-spec nil nil JetBrains\ Mono nil nil nil nil nil nil nil nil nil nil>))))

(after! evil-snipe
  (setq evil-snipe-scope 'whole-visible)
  (setq evil-snipe-repeat-scope nil)  ;; same as evil-snipe-scope
  )

;; (global-auto-revert-mode 1)
(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      inhibit-compacting-font-caches t)

(setq org-directory "~/.org/")

(after! org
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))
        org-ellipsis " ▼"
        ))

(use-package org-id
  :after org
  :config
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
  ;; (add-hook 'auto-save-hook 'org-id-update-id-locations 20)
  )

;; (after! org-roam
;;   (add-hook 'auto-save-hook 'org-roam-build-cache 40))

(after! org
  (setq org-startup-indented 'indent
         org-startup-folded 'fold
         org-startup-with-inline-images t
         )
)
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
      :desc "Prioity up" "C-S-k" #'org-agenda-priority-up
      :desc "Prioity down" "C-S-j" #'org-agenda-priority-down
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

(after! org
  (setq org-priority-default ?D)
  (setq org-priority-lowest ?E)
  )

(setq org-agenda-custom-commands
      '(
        ("b" "Agenda and tasks B+"
         ((agenda "Agenda"
                  ((org-agenda-use-time-grid t)
                   (org-deadline-warning-days 3)
                   (org-agenda-span '1)
                   (org-agenda-start-day (org-today))))
          (org-ql-block '(and (todo "NEXT")
                              (or (priority >= "B")
                                  (ancestors (priority >= "B")))
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
          (org-ql-block '(and (todo "PROJ")
                              (priority >= "B")
                              (not (tags "SOMEDAY"))
                              (not (children (and (todo "NEXT" "WAIT")
                                                  (not (tags "SOMEDAY"))))))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
          (org-ql-block '(and (todo "WAIT")
                              (or (priority >= "B")
                                  (ancestors (priority >= "B")))
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Waiting")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
                        ))
        ("c" "Agenda and tasks C+"
         ((agenda "Agenda"
                  ((org-agenda-use-time-grid t)
                   (org-deadline-warning-days 3)
                   (org-agenda-span '1)
                   (org-agenda-start-day (org-today))))
          (org-ql-block '(and (todo "NEXT")
                              (or (priority >= "C")
                                  (ancestors (priority >= "C")))
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
          (org-ql-block '(and (todo "PROJ")
                              (priority >= "C")
                              (not (tags "SOMEDAY"))
                              (not (children (and (todo "NEXT" "WAIT")
                                                  (not (tags "SOMEDAY"))))))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
          (org-ql-block '(and (todo "WAIT")
                              (or (priority >= "C")
                                  (ancestors (priority >= "C")))
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Waiting")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
                        ))
        ;; ("t" "Tasks"
        ;;  ((org-ql-block '(and (todo)
        ;;                       (not (tags "SOMEDAY"))
        ;;                       (not (and (todo "TODO")
        ;;                                 (ancestors (todo "PROJ"))))
        ;;                       (not (scheduled))
        ;;                       (not (deadline)))
        ;;                 ((org-super-agenda-groups stfl/org-super-agenda-groups)
        ;;                  (org-ql-block-header "Tasks"))
        ;;                 )))
        ("a" "Agenda Weekly"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)))))
        ("r" . "Weekly Review")
        ("rc" "Close the last week and finish done tasks"
         ((org-ql-block '(and (todo "NEXT")
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
          (org-ql-block '(and (todo "WAIT")
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Waiting")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
          (org-ql-block '(and (todo "PROJ")
                              (not (tags "SOMEDAY"))
                              (not (children (and (todo "NEXT" "WAIT")
                                                  (not (tags "SOMEDAY"))))))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
                        ))
        ("rl" "Agenda Weekly with Log"
         ;; TODO add archive!
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-archives-mode t)
                   (org-agenda-start-with-log-mode '(closed))
                   (org-agenda-show-log t)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^.*DONE "))
                   ))))
        ("rp" "Plan Projects"
         ((org-ql-block '(and (todo "PROJ")
                              (not (tags "SOMEDAY"))
                              ;; (not (children "SOMEDAY")))
                              (not (children (and (todo "NEXT" "WAIT")
                                                  (tags "SOMEDAY"))))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Projects")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 14)
                         (org-super-agenda-groups '((:auto-priority)))))))
        ("rn" "Plan Next Actions"
         ((org-ql-block '(and (todo "NEXT")
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit))
                              (or (not (deadline))
                                  (deadline auto)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))
                        ))
        ("rs" "Stuck Projects"
         ((org-ql-block '(and (todo "PROJ")
                              (not (tags "SOMEDAY"))
                              (not (children (and (todo "NEXT" "WAIT")
                                                  (not (tags "SOMEDAY"))))))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:auto-priority)))))))
        ("rS" "SOMEDAY"
         ((org-ql-block '(and (todo "PROJ")
                              (or (and (priority <= "D")
                                       (not (ancestors (priority > "D")))
                                       (not (children (priority > "D"))))
                                  (tags "SOMEDAY")
                                  (children (and (todo "NEXT" "WAIT")
                                                 (tags "SOMEDAY")))
                                  )
                              (not (scheduled))
                              (not (habit))
                              (not (deadline)))
                        ((org-ql-block-header "Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:tag "SOMEDAY" :order 10)
                                                    (:auto-priority)
                                                    ))))
         (org-ql-block '(and (todo "NEXT")
                              (or (and (priority <= "D")
                                  (not (ancestors (priority > "D"))))
                                  (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit))
                              (not (deadline)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:tag "SOMEDAY" :order 10)
                                                    (:auto-priority)
                                                    ))))
          ;; (org-ql-block '(and (todo "PROJ")
          ;;                     (not (tags "SOMEDAY"))
          ;;                     (not (children (and (todo "NEXT" "WAIT")
          ;;                                         (not (tags "SOMEDAY"))))))
          ;;               ((org-ql-block-header "Stuck Projects")
          ;;                (org-super-agenda-header-separator "")
          ;;                (org-super-agenda-groups '((:auto-priority)))))
          ;; (org-ql-block '(and (todo "WAIT")
          ;;                     (not (tags "SOMEDAY"))
          ;;                     (not (scheduled))
          ;;                     (not (deadline)))
          ;;               ((org-ql-block-header "Waiting")
          ;;                (org-super-agenda-header-separator "")
          ;;                (org-super-agenda-groups '((:auto-priority)))))
                        ))
        ("ro" "Old Agenda and tasks"
         ((agenda "Agenda"
                  ((org-agenda-use-time-grid t)
                   (org-deadline-warning-days 3)
                   (org-agenda-span '1)
                   (org-agenda-start-day (org-today))))
          (org-ql-block '(and (todo)
                              (not (tags "SOMEDAY"))
                              (not (and (todo "TODO")
                                        (ancestors (todo "PROJ"))))
                              (not (scheduled))
                              (not (deadline)))
                        ((org-super-agenda-groups stfl/org-super-agenda-groups)
                         ;; TODO super-agenda to separate priorities
                         (org-ql-block-header "Tasks")
                         ;; TODO sort not yet possible: https://github.com/alphapapa/org-ql/pull/44
                         )
                        )))
        ))

(setq org-agenda-diary-file "~/.org/diary.org"
      ;; org-agenda-dim-blocked-tasks t
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-use-time-grid t
      ;; org-agenda-hide-tags-regexp "\\w+"
      org-agenda-compact-blocks nil
      org-agenda-block-separator ""
      org-agenda-tags-column 0
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-unavailable-files t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-window-setup 'current-window
      org-agenda-start-on-weekday nil
      org-agenda-span 'day
      org-agenda-start-day "-0d"
      org-deadline-warning-days 7
      org-agenda-show-future-repeats t
      org-agenda-skip-deadline-prewarning-if-scheduled nil
      org-agenda-tags-todo-honor-ignore-options 1
      ;; org-agenda-todo-ignore-with-date nil
      ;; org-agenda-todo-ignore-deadlines nil
      ;; org-agenda-todo-ignore-timestamp nil
      org-agenda-todo-list-sublevels t
      org-agenda-include-deadlines t
      )

(after! org (setq
                  org-enforce-todo-checkbox-dependencies nil
                  org-enforce-todo-dependencies nil
                  org-habit-show-habits t))

(after! org (setq org-agenda-files '("~/.org/gtd/inbox.org"
                                     ;; "~/.org/gtd/someday.org"
                                     "~/.org/gtd/tickler.org"
                                     "~/.org/calendar.org"
                                     "~/.org/gtd/todo.org"
                                     "~/.org/jira/active.org"
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
  :after (org-agenda evil-org-agenda)
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-separator "")

  (setq stfl/org-super-agenda-groups
        '((:name "Today"
           :deadline past
           :deadline today
           :scheduled today
           :scheduled past)
          (:name "Next Actions" :todo "NEXT")
          (:name "Waiting" :todo "WAIT")
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

  ;; Update ‘org-super-agenda-header-map’

(setq org-super-agenda-header-map evil-org-agenda-mode-map))

(defun stfl/org-ql-min-ancestor-priority< (a b)
  "Return non-nil if A's minimum ancestor priority is higher than B's.
A and B are Org headline elements.
org-default-priority is treated as lower than the same set value"
  (cl-macrolet ((priority (item)
                          `(org-with-point-at (org-element-property :org-marker ,item)
                             (stfl/org-min-ancestor-priority))))
    ;; NOTE: Priorities are numbers in Org elements.  This might differ from the priority selector logic.
    (let ((a-priority (priority a))
          (b-priority (priority b)))
      (cond ((and a-priority b-priority)
             (< a-priority b-priority))
            (a-priority t)
            (b-priority nil)))))


(defun stfl/org-min-ancestor-priority ()
  (cl-loop minimize (save-match-data (stfl/org-priority-or-default))
           while (and (not (equal "PROJ" (nth 2 (org-heading-components))))
                      (org-up-heading-safe))))


(defun stfl/org-priority-or-default ()
  (let* ((prio-raw (org-element-property :priority (org-element-at-point)))
         (prio (cond (prio-raw prio-raw)
                     (t (+ 0.5 org-priority-default))))) ;;
    display empty prio below default prio))

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
  (setq org-image-actual-width 400
        org-archive-location "~/.org/gtd/archive/%s::datetree"
        ))

(use-package! org-habit
  :after org-agenda
  :config
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-preceding-days 6
        org-habit-following-days 7)

  ;; Length of the habit graph
  (setq org-habit-graph-column 65))

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

(after! org
  (setq org-log-into-drawer t
        org-log-done 'time+note
        org-log-repeat 'time
        org-log-redeadline 'time
        org-log-reschedule 'time
        ))

(after! org
  (setq org-use-property-inheritance t ; We like to inherit properties from their parents
        org-catch-invisible-edits 'error ; Catch invisible edits
        org-track-ordered-property-with-tag t
        org-hierarchical-todo-statistics nil
        ))

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

(after! org
  (setq org-tag-alist '((:startgrouptag)
                        ("Context" . nil)
                        (:grouptags)
                        ("@home" . ?h)
                        ("@office". ?o)
                        ("@sarah" . ?s)
                        ("@robert" . ?r)
                        ("@baudock_meeting" . ?b)
                        ("@PC" . ?p)
                        ("@phone" . ?f)
                        (:endgrouptag)
                        ;; (:startgrouptag)
                        ;; ("Categories" . nil)
                        ;; (:grouptags)
                        ;; ("wohnung")
                        ;; ("health")
                        ;; ("bike")
                        ;; ("friends")
                        ;; ("emacs")
                        ;; ("gtd")
                        ;; ("shopping")
                        ;; ("learning")
                        ;; (:endgrouptag)
                        (:startgrouptag)
                        ("Process" . nil)
                        (:grouptags)
                        ("SOMEDAY" . ?S)
                        ("REFILE" . ?R)
                        ("TICKLER" . ?T)
                        ("DELIGATED" . ?D)
                        ("HABIT" . ?H)
                        (:endgrouptag)
                        (:startgrouptag)
                        ("Areas" . nil)
                        (:grouptags)
                        ("pulswerk" . ?$)
                        ("#personal" . ?_)
                        )))

(use-package! org-edna
  :after org)

(add-hook 'org-mode-hook 'org-edna-mode)

;; (defun gtd/planning-trigger ()
;;   "Automatically schedule an entry when it becomes NEXT according to PLANNED property"
;;   (when (equal org-state "NEXT")
;;     (message "das war next")
;;     (setq planned (car (org-map-entries (lambda () (
;;       org-entry-get nil  "PLANNED")) "PLANNED<>\"\"" 'tree)))
;;     (if planned (
;;       (message "Geplant ist %s" planned)
;;       (org-entry-put nil "SCHEDULED" planned)
;;       (org-entry-delete nil "PLANNED")
;;   ) nil) ))

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

;; (use-package! origami)

;; (map! :after '(org-agenda origami)
;;       :map org-agenda-mode-map
;;       :desc "" "TAB" #'origami-toggle-node
;;       ;; :desc "" "" #'org-agenda-priority-tree-down
;;       )

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
  (setq lsp-intelephense-licence-key (get-auth-info "intelephense" "ste.lendl@gmail.com"))
  (setq lsp-intelephense-files-associations '["*.php" "*.phtml" "*.inc"])
  (setq lsp-intelephense-files-exclude '["**update.php**" "**/js/**" "**/fonts/**" "**/gui/**" "**/upload/**"
                                         "**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"])
        ;; (get-auth-info "intelephense" "sutter"))
  ;; (setq lsp-intelephense-trace-server "verbose")
  ;; (setq lsp-intelephense-multi-root t)
  ;; (setq lsp-intelephense-clear-cache t)
  (setq lsp-auto-guess-root nil)
  (setq lsp-idle-delay 0.8)
  )

(add-to-list 'auto-mode-alist '("\\.mq[45h]\\'" . cpp-mode))

(use-package! gitlab-ci-mode
  :mode ".gitlab-ci.yml"
  )

(use-package! gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :init
  (gitlab-ci-mode-flycheck-enable))

;; (use-package kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview))

;; (use-package! kubernetes-evil
;;   :ensure t
;;   :after kubernetes)

(use-package! sql-indent
  :after sql-mode
)

(use-package! ztree)

(after! forge (setq forge-topic-list-columns
                    '(("#" 5 t (:right-align t) number nil)
                      ("Title" 60 t nil title  nil)
                      ("State" 6 t nil state nil)
                      ("Marks" 8 t nil marks nil)
                      ("Labels" 8 t nil labels nil)
                      ("Assignees" 10 t nil assignees nil)
                      ("Updated" 10 t nill updated nil))))

;; (after! todoist (setq todoist-token (get-auth-info "todoist" "stfl")))

;; (use-package! ejira
;;   ;; :after org
;;   :init
;;   (setq jiralib2-url              "https://pulswerk.atlassian.net"
;;         jiralib2-auth             'token
;;         jiralib2-user-login-name  "lendl@pulswerk.at"
;;         jiralib2-token            (get-auth-info "pulswerk.atlassian.net" "lendl@pulswerk.at")

;;         ejira-org-directory       "~/.org/ejira"
;;         ejira-projects            '("MD")

;;         ejira-priorities-alist    '(("Highest" . ?A)
;;                                     ("High"    . ?B)
;;                                     ("Medium"  . ?C)
;;                                     ("Low"     . ?D)
;;                                     ("Lowest"  . ?E))
;;         ejira-todo-states-alist   '(("To Do"       . 1)
;;                                     ("In Progress" . 2)
;;                                     ("Testing" . 3)
;;                                     ("Done"        . 4)))
;;   :config
;;   ;; Tries to auto-set custom fields by looking into /editmeta
;;   ;; of an issue and an epic.
;;   (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

;;   ;; They can also be set manually if autoconfigure is not used.
;;   ;; (setq ejira-sprint-field       'customfield_10001
;;   ;;       ejira-epic-field         'customfield_10002
;;   ;;       ejira-epic-summary-field 'customfield_10004)

;;   (require 'ejira-agenda))

(use-package! org-jira
  :after org
  :init (setq org-jira-working-dir "~/.org/jira/"
              jiralib-url "https://pulswerk.atlassian.net")
        ;; (defconst org-jira-progress-issue-flow
        ;;     '(("To Do" . "In Progress"
        ;;     ("In Progress" . "Done"))))
  :config (setq ;; org-jira-use-status-as-todo t
                org-jira-jira-status-to-org-keyword-alist '(("To Do" . "TODO")
                                                            ("Planned" . "NEXT")
                                                            ("In Progress" . "NEXT")
                                                            ("Testing" . "TEST")
                                                            ("Staging" . "DONE")
                                                            ("Done" . "DONE"))

          org-jira-custom-jqls
                '(
                  (:jql "
assignee='Stefan Lendl'
AND status != Done
AND Sprint in openSprints()
ORDER BY priority, created DESC
"
                   :limit 20
                   :filename "active")))
  )

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
