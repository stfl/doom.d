(setq user-full-name "Stefan Lendl"
      user-mail-address "ste.lendl@gmail.com")

(setq! auth-sources '("~/.config/authinfo/authinfo.gpg")
       ;; auth-source-cache-expiry nil ; default is 7200 (2h)
       auth-source-gpg-encrypt-to nil)

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

;; (when (equal (window-system) nil)
;;   (and
;;    (bind-key "C-<down>" #'+org/insert-item-below)
;;    (setq doom-theme 'doom-solarized-dark)
;;    (setq doom-font (font-spec :family "Source Code Pro" :size 20))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; (setq doom-font (font-spec :family "Fira Code" :size 13)
;;       doom-variable-pitch-font (font-spec :family "Fira Code")
;;       doom-unicode-font (font-spec :family "Fira Code")
;;       doom-big-font (font-spec :family "Fira Code Medium" :size 20))

(if (string= system-name "lendl-fujitsu")
    (setq doom-font (font-spec :family "JetBrainsMono" :size 27 :weight 'light)
          doom-big-font (font-spec :family "JetBrainsMono" :size 36))
      ;; doom-variable-pitch-font (font-spec :family "JetBrainsMono" :size 27)
      ;; doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))
;; (setq doom-font (font-spec :family "JetBrains Mono" :size (round (/ display-pixels-per-inch 2.7)))  ; 27
;;       doom-variable-pitch-font (font-spec :family "JetBrains Mono")
;;       doom-unicode-font (font-spec :family "JetBrains Mono")
;;       doom-big-font (font-spec :family "JetBrains Mono" :size 40))
                                        ; set a smaller font all non-hidpi workstations
(setq doom-font (font-spec :family "JetBrains Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono")
      doom-big-font (font-spec :family "JetBrains Mono" :size 20)))

(setq doom-unicode-font doom-font)

(custom-set-faces!
 '(org-date :foreground "dark goldenrod" :height 0.85)
 '(org-document-title :foreground "#c678dd" :weight bold :height 1.8)
 '(org-drawer :foreground "dark gray" :height 0.8)
 '(org-property-value :height 0.85)
 '(org-ql-view-due-date :foreground "dark goldenrod")
 '(org-special-keyword :foreground "#83898d" :height 0.8)
 '(org-tag :foreground "#83898d" :weight light :height 0.7)
 '(outline-1 :height 1.5)
 '(outline-2 :height 1.25)
 '(outline-3 :height 1.15)
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

;; (after! org
;;   (setcar org-emphasis-regexp-components "-[:space:]('\"{[:alpha:]")                     ; post
;;   (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]-[:space:].,:!?;'\")}\\[") ; pre
;;   (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
;;   )

(set-popup-rule! "^CAPTURE" :side 'bottom :size 0.40 :select t :ttl nil)

(after! org-ql
  (set-popup-rule!
    "^\\*Org QL View" :side 'left :size 0.40 :select t :quit nil
    ))

;; (use-package! tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (after! tree-sitter-langs
;;   (pushnew! tree-sitter-major-mode-language-alist
;;             '(scss-mode . css)))

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

(after! org-id
  (setq org-id-link-to-org-use-id t
        org-id-locations-file "~/.emacs.d/.local/.org-id-locations"
        org-id-track-globally t)
   (org-id-update-id-locations)
  )

(after! org
  (add-hook 'auto-save-hook 'org-save-all-org-buffers 10)
  ;; (add-hook 'auto-save-hook 'org-id-update-id-locations 20)
  ;; )

;; (after! org-roam
;;   (add-hook 'auto-save-hook 'org-roam-build-cache 40))

(after! org
  (setq org-startup-indented 'indent
         org-startup-folded 'fold
         org-startup-with-inline-images t
         ;; org-image-actual-width (round (* (font-get doom-font :size) 25))
         org-image-actual-width (* (default-font-width) 40)
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
      "N" #'org-add-note

      :prefix ("s" . "Tree/Subtree")
      ;; :desc "Rifle Org Directory" "/" #'helm-org-rifle-org-directory
      ;; :desc "Rifle Buffer" "B" #'helm-org-rifle-current-buffer
      ;; :desc "Rifle Agenda Files" "A" #'helm-org-rifle-agenda-files
      ;; :desc "Rifle Project Files" "#" #'helm-org-rifle-project-files
      ;; :desc "Rifle Other Project(s)" "$" #'helm-org-rifle-other-files
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
      :desc "Prioity up" "C-S-k" #'org-agenda-priority-up
      :desc "Prioity down" "C-S-j" #'org-agenda-priority-down
      :localleader
      :desc "Filter" "f" #'org-agenda-filter
      :desc "Follow" "F" #'org-agenda-follow-mode
      "o" #'org-agenda-set-property
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

;; (map! ;;:after org-agenda
;;       :map org-agenda-mode-map
;;       )

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
  (setq org-priority-default ?E)
  (setq org-priority-lowest ?I))

(defun stfl/agenda-query-stuck-projects()
  '(and (todo "PROJ")
        ;; (priority >= "B")
        (not (tags "SOMEDAY"))
        (not (children (and (todo "NEXT" "WAIT")
                            (not (tags "SOMEDAY")))))))

(defvar stfl/agenda-backlog-prio-threshold ?F)

(defun stfl/agenda-ts-now-day+ (adjust-days)
  (ts-format "%Y-%m-%d" (ts-adjust 'day adjust-days (ts-now))))

(defun scheduled-to(days)
  `(or (not (scheduled))
       (scheduled :to ,+days)))

(defun deadline-to(days)
  `(or (not (deadline))
       (deadline :to ,days)
       (ancestors (deadline :to ,days))))

(setq org-agenda-custom-commands
      '(
        ("b" "Agenda and tasks B+"
         ((agenda "Agenda"
                  ((org-agenda-use-time-grid t)
                   (org-deadline-warning-days 1)
                   (org-agenda-span '1)
                   (org-agenda-start-day (org-today))))
          (org-ql-block '(and (todo "NEXT")
                              (or (and (tags "org_jira")        ; for org-jira I need to also check the assignee
                                       (property "assignee" "Stefan Lendl"))

                                  (and (not (tags "org_jira"))  ; otherwise I consider the regular org priority
                                       (or (priority >= "B")
                                           (ancestors (priority >= "B"))
                                           (deadline auto)
                                           (ancestors (deadline auto)))))
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 7)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
          (org-ql-block (stfl/agenda-query-stuck-projects)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))
          (org-ql-block '(and (todo "WAIT")
                              (or (priority >= "B")
                                  (ancestors (priority >= "B"))
                                  (deadline auto)
                                  (ancestors (deadline auto)))
                              (not (tags "SOMEDAY"))
                              (not (scheduled)))
                        ((org-ql-block-header "Waiting")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 7)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
          ))
        ("c" "Agenda and tasks C+"
         ((agenda "Agenda"
                  ((org-agenda-use-time-grid t)
                   (org-deadline-warning-days 1)
                   (org-agenda-span '1)
                   (org-agenda-start-day (org-today))))
          (org-ql-block '(and (todo "NEXT")
                              (or (and (tags "org_jira")        ; for org-jira I need to also check the assignee
                                       (property "assignee" "Stefan Lendl"))
                                  (and (not (tags "org_jira"))  ; otherwise I consider the regular org priority
                                       (or (priority >= "C")
                                           (ancestors (priority >= "C"))
                                           (deadline auto)
                                           (ancestors (deadline auto)))))
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 14)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
          (org-ql-block (stfl/agenda-query-stuck-projects)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))
          (org-ql-block '(and (todo "WAIT")
                              (or (priority >= "C")
                                  (ancestors (priority >= "C"))
                                  (deadline auto)
                                  (ancestors (deadline auto)))
                              (not (tags "SOMEDAY"))
                              (not (scheduled)))
                        ((org-ql-block-header "Waiting")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 14)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
          ))
        ("d" "Agenda and tasks D+"
         ((agenda "Agenda"
                  ((org-agenda-use-time-grid t)
                   (org-deadline-warning-days 1)
                   (org-agenda-span '1)
                   (org-agenda-start-day (org-today))))
          (org-ql-block '(and (todo "NEXT")
                              (or (and (tags "org_jira")        ; for org-jira I need to also check the assignee
                                       (property "assignee" "Stefan Lendl"))
                                  (and (not (tags "org_jira"))  ; otherwise I consider the regular org priority
                                       (or (priority >= "D")
                                           (ancestors (priority >= "D"))
                                           (deadline auto)
                                           (ancestors (deadline auto)))))
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 14)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
          (org-ql-block (stfl/agenda-query-stuck-projects)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))
          (org-ql-block '(and (todo "WAIT")
                              (or (priority >= "D")
                                  (ancestors (priority >= "D"))
                                  (deadline auto)
                                  (ancestors (deadline auto)))
                              (not (tags "SOMEDAY"))
                              (not (scheduled)))
                        ((org-ql-block-header "Waiting")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 14)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
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
        ("rc" "Close open NEXT Actions and WAIT"
         ((org-ql-block '(and (todo "NEXT" "WAIT")
                              (not (tags "SOMEDAY" "HABIT" "org_jira"))
                              (not (habit))
                              (or (not (deadline))
                                  (deadline :to +30)
                                  (ancestors (deadline :to +30)))
                              (or (not (scheduled))
                                  (scheduled :to +30))
                              )
                        ((org-super-agenda-header-separator "")
                         (org-deadline-warning-days 30)
                         (end (ts-format (ts-adjust 'day 60 (ts-now))))
                         (org-super-agenda-groups stfl/priority-groups)
                         (org-ql-block-header "Something to do")
                         ))
          (org-ql-block (stfl/agenda-query-stuck-projects)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))
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
                                  (deadline auto)
                                  (ancestors (deadline auto))))
                        ((org-ql-block-header "Projects")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 45)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
          ))
        ("rn" "Plan Next Actions"
         ((org-ql-block '(and (todo "NEXT")
                              (not (tags "SOMEDAY"))
                              (not (scheduled))
                              (not (habit))
                              (or (not (deadline))
                                  (deadline auto)
                                  (ancestors (deadline auto))))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-header-separator "")
                         (org-deadline-warning-days 14)
                         (org-super-agenda-groups stfl/priority-groups)
                         ))
          ))
        ("rs" "Stuck Projects"
         ((org-ql-block (stfl/agenda-query-stuck-projects)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))
          ))
        ("rS" "SOMEDAY"
         ((org-ql-block '(and (todo "PROJ")
                              (or (and (priority <= (char-to-string stfl/agenda-backlog-prio-threshold))
                                       (not (ancestors (priority > (char-to-string stfl/agenda-backlog-prio-threshold))))
                                       (not (children (priority > (char-to-string stfl/agenda-backlog-prio-threshold)))))
                                  (tags "SOMEDAY")
                                  (children (and (todo "NEXT" "WAIT")
                                                 (tags "SOMEDAY"))))
                              (not (scheduled))
                              (not (habit))
                              (not (deadline)))
                        ((org-ql-block-header "Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups '((:tag "SOMEDAY" :order 10)
                                                    (:auto-priority)
                                                    ))))
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

;; (defun stfl/agenda-next-wait ()
;;   (interactive)
;;   (let* ((ts-default-format "%Y-%m-%d")
;;          (end (ts-format (ts-adjust 'day 60 (ts-now)))))
;;     (org-ql-search (org-agenda-files)
;;       `(and (todo "NEXT" "WAIT")
;;             (not (tags "org_jira" "SOMEDAY" "TICKLER" "HABIT"))
;;             ;; (not (children (and (todo "NEXT" "WAIT")
;;             ;;                     (not (tags "SOMEDAY")))))
;;             (or (not (scheduled))
;;                 (scheduled :to ,end))
;;             (or (not (deadline))
;;                 (deadline auto)
;;                 (ancestors (deadline auto))))
;;       :title "Weekly Review"
;;       :super-groups stfl/priority-groups)))

;; ;; Past week intraspection
;; (defun past-week-range (num)
;;   "Return timestamps (BEG . END) spanning the previous*NUM calendar week."
;;   (let* (;; Bind `now' to the current timestamp to ensure all calculations
;;          ;; begin from the same timestamp.  (In the unlikely event that
;;          ;; the execution of this code spanned from one day into the next,
;;          ;; that would cause a wrong result.)
;;          (now (ts-now))
;;          ;; We start by calculating the offsets for the beginning and
;;          ;; ending timestamps using the current day of the week.  Note
;;          ;; that the `ts-dow' slot uses the "%w" format specifier, which
;;          ;; counts from Sunday to Saturday as a number from 0 to 6.
;;          (adjust-beg-day (- (+ (* num 7) (ts-dow now))))
;;          (adjust-end-day (- (- (* num 7) (- 6 (ts-dow now)))))
;;          ;; Make beginning/end timestamps based on `now', with adjusted
;;          ;; day and hour/minute/second values.  These functions return
;;          ;; new timestamps, so `now' is unchanged.
;;          (beg (thread-last now
;;                            ;; `ts-adjust' makes relative adjustments to timestamps.
;;                            (ts-adjust 'day adjust-beg-day)
;;                            ;; `ts-apply' applies absolute values to timestamps.
;;                            (ts-apply :hour 0 :minute 0 :second 0)))
;;          (end (thread-last now
;;                            (ts-adjust 'day adjust-end-day)
;;                            (ts-apply :hour 23 :minute 59 :second 59))))
;;     (cons beg end)))

;; ;; Weekly Review
;; (defun org-user/week-intraspection ()
;;   (interactive)
;;   (let* ((week-num (read-number "How many weeks in past? " 1))
;;          (ts-default-format "%Y-%m-%d")
;;          (wbeg (ts-format (car (past-week-range week-num))))
;;          (wend (ts-format (cdr (past-week-range week-num)))))
;;     (org-ql-search (org-agenda-files)
;;       `(and (done)
;;             (closed :from ,wbeg :to ,wend))
;;       :title "Weekly Review"
;;       :super-groups (quote ((:auto-ts t))))))

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

(after! org-super-agenda
  (setq stfl/priority-groups
        '((:tag "SOMEDAY" :order 90)
          (:name "[#A] MUST Do this week (<=2)"
           :priority "A"
           :and (:tag "org_jira"
                 :property ("status" "In Progress")))
          (:name "[#B] SHOULD Do this week (<=3)"
           :priority "B"
           :and (:tag "org_jira"
                 :property ("status" "Planned")))
          (:name "[#C] Optional or consider for next week (<=5)"
           :priority "C")
          (:name "[#D] I care a bit more (~8)"
           :priority "D")
          (:name "[#E] (~8)"
           :priority "E")
          (:name "[#F] Priority -1 (~8)"
           :order 81
           :priority "F")
          (:name "[#G] Priority -2 (~8)"
           :order 82
           :priority "G")
          (:name "[#H] Priority -3"
           :order 83
           :priority "H")
          (:name "[#I] Priority -4 Consider for SOMEDAY"
           :order 84
           :priority "I")
          (:name "Default Priority : reduce as much as possible (<=8)"
           :not
           :priority
           ))))

;; (after! org
(setq! org-agenda-diary-file "~/.org/diary.org"
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
       org-agenda-skip-deadline-prewarning-if-scheduled t
       org-agenda-tags-todo-honor-ignore-options 1
       ;; org-agenda-todo-ignore-with-date nil
       ;; org-agenda-todo-ignore-deadlines nil
       ;; org-agenda-todo-ignore-timestamp nil
       org-agenda-todo-list-sublevels t
       org-agenda-include-deadlines t
       org-stuck-projects '("-SOMEDAY/+PROJ" ("NEXT" "WAIT") ("WAITING") ""))

(after! org (setq
                  org-enforce-todo-checkbox-dependencies nil
                  org-enforce-todo-dependencies nil
                  org-habit-show-habits t))

(after! org (setq org-agenda-files '("~/.org/gtd/inbox.org"
                                     ;; "~/.org/gtd/someday.org"
                                     "~/.org/gtd/tickler.org"
                                     "~/.org/calendar.org"
                                     "~/.org/gtd/todo.org"
                                     "~/.org/jira/"
                                     "~/.org/gtd/projects/")))
;; (append (file-expand-wildcards "~/.org/gtd/*.org")
;;         (file-expand-wildcards "~/.org/gtd/projects/*.org"))))

;; (after! org
;;   (setq org-agenda-files '("~/.org/gtd/inbox.org"
;;                            "~/.org/gtd/projects.org"
;;                            "~/.org/gtd/tickler.org"))

(after! org
  (setq! org-clock-continuously t))

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

(after! org
  (setq org-capture-templates
        '(
          ("n" "capture to inbox"
           entry
           (file+headline "~/.org/gtd/inbox.org" "Inbox")
           (file "~/.doom.d/templates/template-inbox.org"))
          ("p" "Project"
           entry
           (file+headline "~/.org/gtd/inbox.org" "Inbox")
           (file "~/.doom.d/templates/template-projects.org")
           :empty-lines-after 1)
          ("s" "scheduled"
           entry
           (file+headline "~/.org/gtd/inbox.org" "Inbox")
           (file "~/.doom.d/templates/template-scheduled.org"))
          ("S" "deadline"
           entry
           (file+headline "~/.org/gtd/inbox.org" "Inbox")
           (file "~/.doom.d/templates/template-inbox.org"))
          ("P" "Protocol"
           entry
           (file+headline "~/.org/gtd/inbox.org" "Inbox")
           "* %^{Title}\nSource: [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
           :empty-lines-after 1)
          ("L" "Protocol Link"
           entry
           (file+headline "~/.org/gtd/inbox.org" "Inbox")
           "* [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
           :empty-lines-after 1)
          )
        ))

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
  (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )

(after! org
  (setq
   ;; org-image-actual-width 400
   org-archive-location "~/.org/gtd/archive/%s::datetree"
   ))

(use-package! org-habit
  :after org-agenda
  :config
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-preceding-days 14
        org-habit-following-days 7)

  ;; Length of the habit graph
  (setq org-habit-graph-column 31))

(use-package! org-edna
  :after org
  ;; :hook org-mode-hook  ;; load package after hook
  ;; :config (org-edna-mode)  ;; enable after load
  )

(add-hook! 'org-mode-hook #'org-edna-mode)

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
           "NEXT(n)"  ; Task is next to be worked on.
           "WAIT(w)"  ; Something external is holding up this task
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
                        ("#work" . ?$)
                        ("#personal" . ?_)
                        ("#emacs" . ?-)
                        )))

(after! org-roam
  (setq org-roam-tag-sources '(prop last-directory)
        org-roam-directory "~/.org/"
        org-roam-db-location "~/.emacs.d/roam.db"
        org-roam-file-exclude-regexp "*/.stversions/*"))

(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil))

;; (after! org-roam (org-roam-db-build-cache))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (use-package! org-roam-server
;;   ;; :ensure t
;;   :after org-roam
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 7070
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-poll nil
;;         org-roam-server-network-arrows 'from
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20)

;;   ;; (org-roam-server-mode)
;;   )

(after! org-gcal
;; (use-package! org-gcal
  (setq org-gcal-client-id (get-auth-info "org-gcal-client-id" "ste.lendl@gmail.com")
        org-gcal-client-secret (get-auth-info "org-gcal-client-secret" "ste.lendl@gmail.com")
        org-gcal-fetch-file-alist '(("ste.lendl@gmail.com" .  "~/.org/calendar.org"))
        org-gcal-token-file "~/.config/authinfo/org-gcal-token.gpg"
        org-gcal-down-days 180
        ))

(map!
 :map org-mode-map
 :leader
 (:prefix ("n" . "notes")
  (:prefix ("j" . "sync")
   :desc "sync Google Calendar" "g" #'org-gcal-sync)))

(use-package! ob-mermaid
  :after org
  :init
        (setq ob-mermaid-cli-path "/home/stefan/.yarn/bin/mmdc")
  :config
        (add-to-list 'org-babel-load-languages '(mermaid . t))
  )

(map! :after org
      :map org-mode-map
      :leader
      (:prefix ("n" . "notes")
       (:prefix ("j" . "sync")
        :desc "resolve syncthing conflicts" "c" #'stfl/resolve-orgzly-syncthing
        )))

(defun stfl/resolve-orgzly-syncthing ()
  (interactive)
  (ibizaman/syncthing-resolve-conflicts "~/.org/"))

(defun ibizaman/syncthing-resolve-conflicts (directory)
  "Resolve all conflicts under given DIRECTORY."
  (interactive "D")
  (let* ((all (ibizaman/syncthing--get-sync-conflicts directory))
         (chosen (ibizaman/syncthing--pick-a-conflict all)))
    (ibizaman/syncthing-resolve-conflict chosen)))


(defun ibizaman/syncthing-show-conflicts-dired (directory)
  "Open dired buffer at DIRECTORY showing all syncthing conflicts."
  (interactive "D")
  (find-name-dired directory "*.sync-conflict-*"))

(defun ibizaman/syncthing-resolve-conflict-dired (&optional arg)
  "Resolve conflict of first marked file in dired or close to point with ARG."
  (interactive "P")
  (let ((chosen (car (dired-get-marked-files nil arg))))
    (ibizaman/syncthing-resolve-conflict chosen)))

(defun ibizaman/syncthing-resolve-conflict (conflict)
  "Resolve CONFLICT file using ediff."
  (let* ((normal (ibizaman/syncthing--get-normal-filename conflict)))
    (ibizaman/ediff-files
     (list conflict normal)
     `(lambda ()
        (when (y-or-n-p "Delete conflict file? ")
          (kill-buffer (get-file-buffer ,conflict))
          (delete-file ,conflict))))))

(defun ibizaman/syncthing--get-sync-conflicts (directory)
  "Return a list of all sync conflict files in a DIRECTORY."
  (seq-filter (lambda (o) (not (string-match "\\.stversions" o))) (directory-files-recursively directory "\\.sync-conflict-")))

(defvar ibizaman/syncthing--conflict-history nil
  "Completion conflict history")

(defun ibizaman/syncthing--pick-a-conflict (conflicts)
  "Let user choose the next conflict from CONFLICTS to investigate."
  (completing-read "Choose the conflict to investigate: " conflicts
                   nil t nil ibizaman/syncthing--conflict-history))

(defun ibizaman/syncthing--get-normal-filename (conflict)
  "Get non-conflict filename matching the given CONFLICT."
  (replace-regexp-in-string "\\.sync-conflict-.*\\(\\..*\\)$" "\\1" conflict))

(defun ibizaman/ediff-files (&optional files quit-hook)
  (interactive)
  (lexical-let ((files (or files (dired-get-marked-files)))
                (quit-hook quit-hook)
                (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (when quit-hook (funcall quit-hook))
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(use-package! org-jira
  :after org
  :init (setq org-jira-working-dir "~/.org/jira/"
              jiralib-url "https://pulswerk.atlassian.net")
  ;; (defconst org-jira-progress-issue-flow
  ;;     '(("To Do" . "In Progress"
  ;;     ("In Progress" . "Done"))))
  :config
  (setq org-jira-jira-status-to-org-keyword-alist '(("To Do" . "TODO")
                                                    ("Planned" . "NEXT")
                                                    ("In Progress" . "NEXT")
                                                    ("Staging" . "DONE")
                                                    ("Ready" . "DONE")
                                                    ("Done" . "DONE")
                                                    ("Released" . "DONE"))
        org-jira-priority-to-org-priority-alist (list (cons "Highest" ?A)
                                                      (cons "High" ?C)
                                                      ;; (cons "Medium" ?E)  ;; no org priority for /default/
                                                      (cons "Low" ?E)
                                                      (cons "Lowest" ?G))

        org-jira-custom-jqls '((:jql "
assignee='Stefan Lendl'
AND (Sprint in openSprints()
     OR (Project = MD
         AND status != Done))
ORDER BY priority, created DESC
"
           :limit 300
           :filename "active")))

  (map!
   :map org-mode-map
   :localleader
   :prefix ("j" . "Jira")
   :desc "Get issues from JQL" "j" #'org-jira-get-issues-from-custom-jql
   "n" #'org-jira-create-issue
   "t" #'org-jira-progress-issue
   "T" #'org-jira-progress-issue-next
   "a" #'org-jira-assign-issue
   "r" #'org-jira-refresh-issue
   "b" #'org-jira-refresh-issues-in-buffer
   "u" #'org-jira-update-issue
   "S" #'org-jira-create-subtask
   "s" #'org-jira-get-subtasks
   "N" #'org-jira-todo-to-jira
   (:prefix ("w" . "Worklogs")
    "c" #'org-jira-update-worklogs-from-org-clocks
    "u" #'org-jira-update-worklogs
    "i" #'org-jira-update-worklogs-for-issue)
   (:prefix ("c" . "Comments")
    :desc "Add Comment" "c" #'org-jira-add-comment
    :desc "Update Comment" "u" #'org-jira-update-comment))

  (map!
   :map org-jira-map
   :leader
   (:prefix ("n" . "notes")
    (:prefix ("j" . "sync")
     :desc "Get issues from JQL" "j" #'org-jira-get-issues-from-custom-jql))))

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
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (setq projectile-files-cache-expire 30)
  )

(set-email-account! "gmail"
  '((mu4e-sent-folder       . "/gmail/[Google Mail]/Gesendet")
    (mu4e-drafts-folder     . "/gmail/[Google Mail]/Entw&APw-rfe")
    (mu4e-trash-folder      . "/gmail/[Google Mail]/Trash")
    (mu4e-refile-folder     . "/gmail/[Google Mail]/Alle Nachrichten")
    (smtpmail-smtp-user     . "ste.lendl@gmail.com")
    ;; (+mu4e-personal-addresses . "ste.lendl@gmail.com")
    ;; (mu4e-compose-signature . "---\nStefan Lendl")
    )
  t)

(set-email-account! "pulswerk"
  '((mu4e-sent-folder       . "/pulswerk/Sent Items")
    (mu4e-drafts-folder     . "/pulswerk/Drafts")
    (mu4e-trash-folder      . "/pulswerk/Deleted Items")
    (mu4e-refile-folder     . "/pulswerk/Archive")
    (smtpmail-smtp-user     . "lendl@pulswerk.at")
    ;; (+mu4e-personal-addresses . "lendl@pulswerk.at")
    ;; (mu4e-compose-signature . "---\nStefan Lendl")
    )
  t)

(after! mu4e
  ;; (setq +mu4e-gmail-accounts '(("ste.lendl@gmail.com" . "/gmail")))
  (setq mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask)

  (setq mu4e-maildir-shortcuts
    '((:key ?g :maildir "/gmail/Inbox"   )
      (:key ?p :maildir "/pulswerk/INBOX")
      (:key ?u :maildir "/gmail/Categories/Updates")
      (:key ?j :maildir "/pulswerk/Jira"  )
      (:key ?l :maildir "/pulswerk/Gitlab" :hide t)
      ))

  (setq mu4e-bookmarks
        '(
          (:key ?i :name "Inboxes" :query "not flag:trashed and (m:/gmail/Inbox or m:/pulswerk/INBOX)")
          (:key ?u :name "Unread messages"
           :query
           "flag:unread and not flag:trashed and (m:/gmail/Inbox or m:/gmail/Categories/* or m:/pulswerk/INBOX or m:\"/pulswerk/Pulswerk Alle\" or m:/pulswerk/Jira or m:/pulswerk/Gitlab)")
          (:key ?p :name "pulswerk Relevant Unread" :query "flag:unread not flag:trashed and (m:/pulswerk/INBOX or m:\"/pulswerk/Pulswerk Alle\" or m:/pulswerk/Jira or m:/pulswerk/Gitlab)")
          (:key ?g :name "gmail Relevant Unread" :query "flag:unread not flag:trashed and (m:/gmail/Inbox or m:/gmail/Categories/*)")
          ;; (:key ?t :name "Today's messages" :query "date:today..now" )
          ;; (:key ?y :name "Yesterday's messages" :query "date:2d..1d")
          ;; (:key ?7 :name "Last 7 days" :query "date:7d..now" :hide-unread t)
          ;; ;; (:name "Messages with images" :query "mime:image/*" :key 112)
          ;; (:key ?f :name "Flagged messages" :query "flag:flagged")
          ;; (:key ?g :name "Gmail Inbox" :query "maildir:/gmail/Inbox and not flag:trashed")
          ))
  )

(after! mu4e-alert
  (setq mu4e-alert-interesting-mail-query
           "flag:unread and not flag:trashed and (m:/gmail/Inbox or m:/gmail/Categories/Updates or m:/pulswerk/INBOX or m:\"/pulswerk/Pulswerk Alle\" or m:/pulswerk/Jira or m:/pulswerk/Gitlab)"))

(after! mu4e
  (setq mu4e-headers-fields
        '((:flags . 6)
          (:account-stripe . 2)
          (:from-or-to . 25)
          (:folder . 10)
          (:recipnum . 2)
          (:subject . 80)
          (:human-date . 8))
        +mu4e-min-header-frame-width 142
        mu4e-headers-date-format "%d/%m/%y"
        mu4e-headers-time-format "⧖ %H:%M"
        mu4e-headers-results-limit 1000
        mu4e-index-cleanup t)

  (defvar +mu4e-header--folder-colors nil)
  (appendq! mu4e-header-info-custom
            '((:folder .
               (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
                (lambda (msg)
                  (+mu4e-colorize-str
                   (replace-regexp-in-string "\\`.*/" "" (mu4e-message-field msg :maildir))
                   '+mu4e-header--folder-colors)))))))

(after! mu4e
  (setq sendmail-program "/usr/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from") ; , "--read-recipients")
        message-send-mail-function #'message-send-mail-with-sendmail))

;; (use-package! mu4e-views
;;   :after mu4e
;;   )

(setq +org-msg-accent-color "#1a5fb4"
      org-msg-greeting-fmt "\nHi %s,\n\n"
      org-msg-signature "\n\n#+begin_signature\n*MfG Stefan Lendl*\n#+end_signature")

(map! :map org-msg-edit-mode-map
      :after org-msg
      :n "G" #'org-msg-goto-body)

(after! ediff
  (setq ediff-diff-options "--text"
        ediff-diff3-options "--text"
        ediff-toggle-skip-similar t
        ediff-diff-options "-w"
        ;; ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; (use-package! origami)

;; (map! :after '(org-agenda origami)
;;       :map org-agenda-mode-map
;;       :desc "" "TAB" #'origami-toggle-node
;;       ;; :desc "" "" #'org-agenda-priority-tree-down
;;       )

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

(add-transient-hook! #'org-babel-execute-src-block
  (require 'ob-async))

(defvar org-babel-auto-async-languages '()
  "Babel languages which should be executed asyncronously by default.")

(defadvice! org-babel-get-src-block-info-eager-async-a (orig-fn &optional light datum)
  "Eagarly add an :async parameter to the src information, unless it seems problematic.
This only acts o languages in `org-babel-auto-async-languages'.
Not added when either:
+ session is not \"none\"
+ :sync is set"
  :around #'org-babel-get-src-block-info
  (let ((result (funcall orig-fn light datum)))
    (when (and (string= "none" (cdr (assoc :session (caddr result))))
               (member (car result) org-babel-auto-async-languages)
               (not (assoc :async (caddr result))) ; don't duplicate
               (not (assoc :sync (caddr result))))
      (push '(:async) (caddr result)))
    result))

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

(after! poetry
  (setq poetry-tracking-strategy 'projectile)
  )

(add-to-list 'auto-mode-alist '("\\.mq[45h]\\'" . cpp-mode))

;; (use-package! gitlab-ci-mode
;;   :mode ".gitlab-ci.yml"
;;   )

;; (use-package! gitlab-ci-mode-flycheck
;;   :after flycheck gitlab-ci-mode
;;   :init
;;   (gitlab-ci-mode-flycheck-enable))

(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :after kubernetes)

(use-package! kubernetes-helm
  :commands kubernetes-helm-status)

(use-package! k8s-mode
  :after yaml-mode
  :hook (k8s-mode . yas-minor-mode))

(use-package! sql-indent
  :after sql-mode
)

(use-package! edbi
  :commands 'edbi:open-db-viewer
  )

(use-package! edbi-minor-mode
  :after sql-mode
  :hook sql-mode-hook
  )
;; (add-hook 'sql-mode-hook 'edbi-minor-mode)

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

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(defun stfl/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.org" doom-private-dir)))

(define-key! help-map "dc" #'stfl/goto-private-config-file)

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

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(after! magit
  (setq magit-diff-refine-hunk 'all))
