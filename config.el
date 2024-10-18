(setq user-full-name "Stefan Lendl"
      user-mail-address "ste.lendl@gmail.com")

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

(defun get-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (when result
      (funcall (plist-get (car result) :secret)))))

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(defun stfl/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.org" doom-private-dir)))

(define-key! help-map "dc" #'stfl/goto-private-config-file)

;; (global-auto-revert-mode 1)
(setq undo-limit 80000000
      evil-want-fine-undo t
      inhibit-compacting-font-caches t)

(setq auto-save-default t)
(run-with-idle-timer 60 t '(lambda () (save-some-buffers t)))

(when (executable-find "brave")
  (setq! browse-url-browser-function 'browse-url-chromium
         browse-url-chromium-program "brave"))

(global-set-key [M-drag-mouse-2] #'mouse-drag-vertical-line)

;; (defun mouse-drag-left-line (start-event)
;;   "Change the width of a window by dragging on a vertical line.
;; START-EVENT is the starting mouse event of the drag action."
;;   (interactive "e")
;;   (mouse-drag-line start-event 'left))

;; (global-set-key [left-fringe drag-mouse-1] #'mouse-drag-left-line)

(after! evil-snipe
  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'visible))

(map! :leader "f ." #'find-file-at-point)

(setq! tab-always-indent 'complete)

(setq doom-theme 'doom-one)

(setq! display-line-numbers-type t)
(setq! which-key-idle-delay 0.3)

(let ((font "JetBrains Mono"))
  (if (string= (system-name) "manjaro.stfl.sh")
      (setq doom-font (font-spec :family font :size 20)
            doom-variable-pitch-font (font-spec :family font)
            doom-big-font (font-spec :family font :size 30))
    (setq doom-font (font-spec :family font :size 13)
          doom-variable-pitch-font (font-spec :family font)
          doom-big-font (font-spec :family font :size 20))))

(custom-declare-face '+org-priority-a  '((t)) "")
(custom-declare-face '+org-priority-b  '((t)) "")
(custom-declare-face '+org-priority-c  '((t)) "")
(custom-declare-face '+org-priority-d  '((t)) "")
(custom-declare-face '+org-priority-e  '((t)) "")
(custom-declare-face '+org-priority-f  '((t)) "")
(custom-declare-face '+org-priority-g  '((t)) "")
(custom-declare-face '+org-priority-h  '((t)) "")
(custom-declare-face '+org-priority-i  '((t)) "")
(custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-epic    '((t (:inherit (bold org-cite org-todo)))) "")
(custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-next    '((t (:inherit (bold font-lock-keyword-face org-todo)))) "")
(custom-declare-face 'org-checkbox-statistics-todo '((t (:inherit (bold font-lock-constant-face org-todo)))) "")

(custom-set-faces!
  '(org-date :foreground "dark goldenrod" :height 0.85)
  '(org-document-title :foreground "#c678dd" :weight bold :height 1.8)
  '(org-drawer :foreground "dark gray" :height 0.8)
  '(org-property-value :height 0.85)
  '(org-ql-view-due-date :foreground "dark goldenrod")
  '(org-special-keyword :foreground "#83898d" :height 0.8)
  '(org-tag :foreground "#83898d" :weight light :height 0.7)
  `(org-code :foreground ,(doom-lighten (doom-color 'warning) 0.3) :extend t)
  '(outline-1 :height 1.5)
  '(outline-2 :height 1.25)
  '(outline-3 :height 1.15)
  `(whitespace-indentation :background ,(doom-color 'base4)) ; Visually highlight if an indentation issue was discovered which emacs already does for us
  `(magit-branch-current  :foreground ,(doom-color 'blue) :box t)
  '(lsp-inlay-hint-face :height 0.85 :italic t :inherit font-lock-comment-face)

  `(+org-todo-cancel :foreground ,(doom-blend (doom-color 'red) (doom-color 'base5) 0.35) :inherit (bold org-done))
  '(+org-priority-a  :foreground "red3" :weight bold :height .95)
  '(+org-priority-b  :foreground "OrangeRed2" :weight bold)
  '(+org-priority-c  :foreground "DarkOrange2" :weight bold)
  '(+org-priority-d  :foreground "gold3" :weight bold)
  '(+org-priority-e  :foreground "OliveDrab1" :weight bold)
  '(+org-priority-f  :foreground "SpringGreen3" :weight bold)
  '(+org-priority-g  :foreground "cyan4" :weight bold)
  '(+org-priority-h  :foreground "DeepSkyBlue4" :weight bold)
  '(+org-priority-i  :foreground "LightSteelBlue3" :weight bold)
)

(after! org

  (setq org-priority-faces
        '((?A . +org-priority-a)
          (?B . +org-priority-b)
          (?C . +org-priority-c)
          (?D . +org-priority-d)
          (?E . +org-priority-e)
          (?F . +org-priority-f)
          (?G . +org-priority-g)
          (?H . +org-priority-h)
          (?I . +org-priority-i))))

(after! org
  (auto-fill-mode))

(custom-set-faces!
  '(adoc-code-face :inherit org-block)
  '(adoc-complex-replacement-face :inherit org-code :bold t)
  '(adoc-meta-face :inherit org-meta-line)
  '(adoc-typewriter-face :inherit org-code)
  '(adoc-verbatim-face :inherit org-verbatim)
  '(adoc-internal-reference-face :inherit org-link)
  '(adoc-reference-face :inherit org-link)
  `(adoc-emphasis-face :foreground ,(doom-lighten (doom-color 'green) 0.2) :slant italic)
  '(adoc-bold-face :bold t)
  `(adoc-command-face :foreground ,(doom-color 'base1) :background ,(doom-color 'base6))
  '(adoc-warning-face :inherit org-warning))

(custom-set-faces!
 '(notmuch-message-summary-face      :foreground "#848d94")  ;; between dooms base6 and base7
 `(notmuch-wash-cited-text           :foreground ,(doom-color 'base6))
 `(notmuch-search-subject            :foreground ,(doom-darken (doom-color 'fg) 0.05))
 '(notmuch-search-unread-face        :weight bold :slant italic)
 `(notmuch-tree-match-tree-face      :foreground              ,(doom-color 'yellow))
 `(notmuch-tree-no-match-tree-face   :foreground              ,(doom-color 'base5))
 `(notmuch-tree-no-match-author-face :foreground ,(doom-darken (doom-color 'blue)    0.3))
 `(notmuch-tree-no-match-date-face   :foreground ,(doom-darken (doom-color 'numbers) 0.3))
 `(notmuch-tree-no-match-tag-face    :foreground ,(doom-darken (doom-color 'yellow)  0.4)))

;; (set-popup-rules!
;;   '(("^\\*subject:" :ignore t)  ; notmuch list view
;;     ("^CAPTURE" :side 'bottom :size 0.40 :select t :ttl nil)
;;     ("^\\*Org Note" :side 'bottom :size 0.40 :select t :ttl nil)
;;     ("^\\*Org QL View" :side 'left :size 0.40 :select t :quit nil)))

(set-popup-rule! "^\\*ein:" :ignore t :quit nil)

(custom-set-faces!
  `(blamer-face :italic t :height 90 :weight semi-light :foreground ,(doom-color 'base5)))

(custom-set-faces!
  `(blamer-face :inherit font-lock-comment-face
    :italic t
    :font "JetBrains Mono"
    :height 0.9
    :background unspecified
    ;; :weight semi-light
    ;; :foreground ,(doom-color 'base5)
    ))

;; (after! (solaire-mode demap)
(use-package! demap
  :commands demap-toggle
  :config
  (setq demap-minimap-window-width 15)
  (let ((gray1 "#1A1C22")
        (gray2 "#21242b")
        (gray3 "#282c34")
        (gray4 "#2b3038") )
    (face-spec-set 'demap-minimap-font-face
                   `((t :background ,gray2
                        :inherit    unspecified
                        :family     "minimap"
                        :height     10          )))
    (face-spec-set 'demap-visible-region-face
                   `((t :background ,gray4
                        :inherit    unspecified )))
    (face-spec-set 'demap-visible-region-inactive-face
                   `((t :background ,gray3
                        :inherit    unspecified )))
    (face-spec-set 'demap-current-line-face
                   `((t :background ,gray1
                        :inherit    unspecified )))
    (face-spec-set 'demap-current-line-inactive-face
                   `((t :background ,gray1
                        :inherit    unspecified ))))

;;   (defun my-track-window-update-p()
;;     "my minimap update predicate function.

;; minimaps only show windows in the same frame"
;;     (and (demap-track-w-mode-update-p-func-default)
;;          (get-buffer-window) ))

;;   (setq demap-track-window-mode-update-p-func #'my-track-window-update-p)

  (map!
   :leader
   :prefix ("t" "+toggle")
   :desc "Minimap" "m" #'demap-toggle)
  )

(after! highlight-indent-guides
  (setq! highlight-indent-guides-auto-character-face-perc 20))

(pixel-scroll-precision-mode)

(setq! tab-width 8)

(setq org-directory "~/.org")

(after! org
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))
        org-ellipsis " ▼"
        ))

(after! org-id
  (setq org-id-link-to-org-use-id t
        org-id-locations-file (doom-path doom-local-dir "org-id-locations")
        org-id-track-globally t))

(after! org-id (run-with-idle-timer 20 nil 'org-id-update-id-locations))

(after! org-roam (run-with-idle-timer 25 nil 'org-roam-update-org-id-locations))

(after! org (run-with-idle-timer 60 t #'org-save-all-org-buffers))

(after! org
  (set-company-backend! 'org-mode
    '(:separate company-capf
      :separate company-org-roam
      :separate company-yasnippet
      :separate company-files)))

(after! org
  (setq org-startup-indented 'indent
        org-startup-folded 'fold
        org-startup-with-inline-images t
        ;; org-image-actual-width (round (* (font-get doom-font :size) 25))
        org-image-actual-width (* (default-font-width) 40)
        ))
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
      "F" #'+org-fix-blank-lines
      "N" #'org-add-note

      :prefix ("l" . "links")
      "o" #'org-open-at-point
      "g" #'eos/org-add-ids-to-headlines-in-file

      :prefix ("d" . "dates/deadlines")
      "c" #'org-cancel-repeater
      )

(after! org
  (setq org-priority-default ?E)
  (setq org-priority-lowest ?I))

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⛔" "𐱄" "▲" "ᐱ" "Ⲷ" "ᐯ" "▼" "𐠠" "҉")))

(defun stfl/build-my-someday-files ()
  (file-expand-wildcards (doom-path org-directory "gtd/someday/*.org")))

(after! org
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 4)
                             (stfl/build-my-someday-files :maxlevel . 4))
        org-refile-use-outline-path 'buffer-name
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))

(defun stfl/build-my-roam-files () (file-expand-wildcards (doom-path org-directory "roam/**/*.org")))

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
  (setq org-capture-templates
        `(("n" "capture to inbox" entry
           (file ,stfl/org-gtd-inbox-absolute)
           (file ,(doom-path doom-private-dir "templates/template-inbox.org"))
           :empty-lines-after 1)
          ("p" "Project" entry
           (file ,stfl/org-gtd-inbox-absolute)
           (file ,(doom-path doom-private-dir "templates/template-projects.org"))
           :empty-lines-after 1)
          ("s" "scheduled" entry
           (file ,stfl/org-gtd-inbox-absolute)
           (file ,(doom-path doom-private-dir "templates/template-scheduled.org"))
           :empty-lines-after 1)
          ("v" "Versicherung" entry
           (file+headline ,(doom-path org-directory "versicherung.org") "Einreichungen")
           (function stfl/org-capture-template-versicherung)
           :root "~/Documents/Finanzielles/Einreichung Versicherung")
          ("S" "deadline" entry
           (file ,stfl/org-gtd-inbox-absolute)
           (file ,(doom-path doom-private-dir "templates/template-deadline.org"))
           :empty-lines-after 1)
          ("P" "Protocol" entry
           (file ,stfl/org-gtd-inbox-absolute)
           "* %^{Title}\nSource: [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
           :empty-lines-after 1)
          ("L" "Protocol Link" entry
           (file ,stfl/org-gtd-inbox-absolute)
           "* [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
           :empty-lines-after 1)
          ("h" "Haushalt")
          ("hw" "Wäsche" entry
           (file+headline ,stfl/org-gtd-todo-absolute "Haushalt")
           (file ,(doom-path doom-private-dir "templates/template-wäsche.org")))
          ))
  )

(after! org-roam
  (setq! org-roam-capture-templates
         `(("d" "default" plain "%?"
            :target (file+head ,(doom-path stfl/org-roam-absolute "%<%Y%m%d%H%M%S>-${slug}.org")
                               "#+title: ${title}\n")
            :unnarrowed t))))

(after! org
  (defun stfl/org-capture-versicherung-post ()
    (unless org-note-abort
      (mkdir (org-capture-get :directory) t)))

  (defun stfl/build-versicherung-dir (root date title)
    (let ((year (nth 5 (parse-time-string date))))
      (format "%s/%d/%s %s" root year date title)))

  (defun stfl/org-capture-template-versicherung ()
    (interactive)
    (let* ((date (org-read-date nil nil nil "Datum der Behandlung" nil nil t))
           (title (read-string "Title: "))
           (directory (stfl/build-versicherung-dir (org-capture-get :root) date title)))
      (org-capture-put :directory directory)
      (add-hook! 'org-capture-after-finalize-hook :local #'stfl/org-capture-versicherung-post)
      (format "* OEGK [%s] %s
:PROPERTIES:
:CREATED: %%U
:date: [%s]
:betrag: %%^{Betrag|0}
:oegk: nil
:generali: nil
:category: %%^{Kategorie|nil|Arzt|Apotheke|Physio|Psychotherapie|Gesundheitsvorsorge}
:END:

[[file:%s]]

%%?" date title date directory)))
)

(after! org (setq org-archive-location (doom-path org-directory "archive/%s::datetree")))

(use-package! org-habit
  :after org-agenda
  :config
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-show-habits t
        org-habit-preceding-days 14
        org-habit-following-days 7
        ;; org-habit-graph-column 31 ;; Length of the habit graph
        ))

(after! org-clock
  (setq org-clock-rounding-minutes 15  ;; Org clock should clock in and out rounded to 5 minutes.
        org-time-stamp-rounding-minutes '(0 15)
        org-duration-format 'h:mm  ;; format hours and don't Xd (days)
        org-clock-report-include-clocking-task t
        org-log-note-clock-out t))

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

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(n)"  ; Task is next to be worked on.
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "LOOP(r)"  ; A recurring task
           "PROJ(p)"  ; Project with multiple task items.
           "EPIC(e)"  ; A set of Projects
           "|"
           "DONE(d@)"  ; Task successfully completed
           "KILL(k@)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(x)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("NEXT" . +org-todo-next)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("EPIC" . +org-todo-epic)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))

(after! org (setq org-indent-indentation-per-level 2))

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

;; (use-package! org-modern
;;   :after org
;;   :config
;;   (setq org-auto-align-tags nil
;;         org-tags-column 0
;;         org-catch-invisible-edits 'show-and-error
;;         org-special-ctrl-a/e t
;;         org-insert-heading-respect-content t

;;         ;; Org styling, hide markup etc.
;;         org-hide-emphasis-markers t
;;         org-pretty-entities t
;;         org-ellipsis "…"

;;         ;; Agenda styling
;;         org-agenda-block-separator ?─
;;         org-agenda-time-grid '((daily today require-timed)
;;                                (800 1000 1200 1400 1600 1800 2000)
;;                                " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;         org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")
;;   )

;; (add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(after! org
  (setq org-tag-alist '((:startgrouptag)
                        ("Context" . nil)
                        (:grouptags)
                        ("@home" . ?h)
                        ("@office". ?o)
                        ("@sarah" . ?s)
                        ;; ("@robert" . ?r)
                        ;; ("@baudock_meeting" . ?b)
                        ;; ("@PC" . ?p)
                        ;; ("@phone" . ?f)
                        (:endgrouptag)
                        (:startgrouptag)
                        ("Process" . nil)
                        (:grouptags)
                        ("SOMEDAY" . ?S)
                        ;; ("REFILE" . ?R)
                        ("HABIT" . ?H)
                        ("LASTMILE" . ?L)
                        ("DRAG" . ?D)
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
        org-roam-directory org-directory
        org-roam-db-location (doom-path doom-local-dir "roam.db")
        org-roam-file-exclude-regexp "\.org/\(?jira\\|\.stversions\)/"))

(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil))

(after! org-roam
  (setq org-roam-dailies-capture-templates
        '(("d" "default"
           entry "* %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n\n"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

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

(after! org-gcal
;; (use-package! org-gcal
  (setq org-gcal-client-id (get-auth-info "org-gcal-client-id" "ste.lendl@gmail.com")
        org-gcal-client-secret (get-auth-info "org-gcal-client-secret" "ste.lendl@gmail.com")
        org-gcal-fetch-file-alist
        `(("ste.lendl@gmail.com" . ,(doom-path org-directory "gcal/stefan.org"))
          ("vthesca8el8rcgto9dodd7k66c@group.calendar.google.com" . ,(doom-path org-directory "gcal/oskar.org")))
        org-gcal-token-file "~/.config/authinfo/org-gcal-token.gpg"
        org-gcal-down-days 180
        ;; org-gcal-auto-archive nil ;; workaround for "rx "**" range error" https://github.com/kidd/org-gcal.el/issues/17
        ))

(map!
 :after (org org-gcal)
 :map org-mode-map
 :leader
 (:prefix ("n" . "notes")
  (:prefix ("j" . "sync")
   :desc "sync Google Calendar" "g" #'org-gcal-sync)))

(map!
 :after (org org-gcal)
 :map org-mode-map
 :localleader
 :prefix ("C" . "Google Calendar")
   :desc "sync Google Calendar" "g" #'org-gcal-sync
   "S" #'org-gcal-sync-buffer
   "p" #'org-gcal-post-at-point
   "d" #'org-gcal-delete-at-point
   "f" #'org-gcal-fetch
   "F" #'org-gcal-fetch-buffer)

(use-package! ob-mermaid
  :after org
  :init
  (setq ob-mermaid-cli-path "/home/stefan/.yarn/bin/mmdc")
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

(use-package! org-jira
  :after org
  :init (setq org-jira-working-dir (doom-path org-directory "jira/")
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
                                                      (cons "Lowest" ?F))

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

(after! org
  (defun individual-visibility-source-blocks ()
    "Fold some blocks in the current buffer with property :hidden"
    (interactive)
    (org-show-block-all)
    (org-block-map
     (lambda ()
       (let ((case-fold-search t))
         (when (and
                (save-excursion
                  (beginning-of-line 1)
                  (looking-at org-block-regexp))
                (cl-assoc
                 ':hidden
                 (cl-third
                  (org-babel-get-src-block-info))))
           (org-hide-block-toggle))))))

  (add-hook 'org-mode-hook #'individual-visibility-source-blocks))

;; (after! org
;;   (setcar org-emphasis-regexp-components "-[:space:]('\"{[:alpha:]")                     ; post
;;   (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]-[:space:].,:!?;'\")}\\[") ; pre
;;   (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
;;   )

;; (use-package! org-pandoc-import :after org)

(after! org-tree-slide (setq org-tree-slide-heading-emphasis nil))

(after! org-tree-slide
  (add-hook 'org-tree-slide-play-hook #'doom-disable-line-numbers-h)
  (add-hook 'org-tree-slide-stop-hook #'doom-disable-line-numbers-h))

(after! org-tree-slide
  (remove-hook 'org-tree-slide-play-hook #'+org-present-hide-blocks-h)
  (remove-hook 'org-tree-slide-stop-hook #'+org-present-hide-blocks-h))

(map! :after org
      :map org-mode-map
      :leader
      (:prefix ("n" . "notes")
       (:prefix ("j" . "sync")
        :desc "resolve syncthing conflicts" "c" #'stfl/resolve-orgzly-syncthing
        )))

(defun stfl/resolve-orgzly-syncthing ()
  (interactive)
  (let ((org-startup-folded 'showeverything)
        (org-inhibit-startup t)
        (org-hide-drawer-startup nil))
    (ibizaman/syncthing-resolve-conflicts org-directory)))

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

;;;###autoload
(defun +org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (let ((heading (org-get-heading t t t t)))
                       ;; (message "Heading: %s" heading)
                       (org-with-wide-buffer
                        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                        ;; newlines before the current heading, so we do this part widened.
                        (cond ((looking-back "^\\*+[^\n]*\n+" nil)
                               (while (looking-back "\n\n" nil)
                                 ;; (message "deleting all empty line in empty subtree")
                                 (backward-char 1)
                                 (delete-char 1)))
                              ((looking-back "\n\n\n+" nil)
                               (while (looking-back "\n\n\n" nil)
                                 ;; (message "deleting double empty lines")
                                 (backward-char 1)
                                 (delete-char 1)))
                              ((not (looking-back "\n\n" nil))
                               ;; (message "inserting newline before heading")
                               (insert "\n"))))
                       (let ((end (org-entry-end-position)))
                         ;; (message "Insert blank lines before entry content")
                         (forward-line)
                         (if (and (org-at-planning-p)
                                  (< (point) (point-max)))
                             ;; Skip planning lines
                             (forward-line))
                         ;; FIXME if there are ONLY planning lines, and now drawer, no \n is inserted
                         (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
                         (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           ;; (message "Insert after drawer")
                           (insert "\n"))))
                     t (if prefix
                           nil
                         'tree)))
  (save-excursion
    (goto-char (point-max))  ; Move to end of buffer
    (cond ((looking-back "^\\*+[^\n]*\n+" nil)
           (while (looking-back "\n\n" nil)
             (backward-char 1)
             (delete-char 1)))
          ((looking-back "\n\n\n+" nil)
           (while (looking-back "\n\n\n" nil)
             (backward-char 1)
             (delete-char 1)))
          ((not (looking-back "\n\n" nil))
           (insert "\n"))))
  (message "Fixed blank lines in org buffer"))

(after! org
  (add-hook 'before-save-hook
            (lambda ()
              (when (and (eq major-mode 'org-mode))
                (+org-fix-blank-lines 4)))))

(after! ws-butler
  (pushnew! ws-butler-global-exempt-modes
            'org-mode))

(map! :after org-agenda
      :map org-agenda-mode-map
      :desc "Prioity up" "C-S-k" #'org-agenda-priority-up
      :desc "Prioity down" "C-S-j" #'org-agenda-priority-down

      :localleader
      "N" #'org-agenda-add-note
      :desc "Filter" "f" #'org-agenda-filter
      :desc "Follow" "F" #'org-agenda-follow-mode
      "o" #'org-agenda-set-property

      :prefix ("p" . "priorities")
      :desc "Prioity" "p" #'org-agenda-priority
      :desc "Prioity up" "u" #'org-agenda-priority-up
      :desc "Prioity down" "d" #'org-agenda-priority-down
      :desc "Someday/Maybe toggle" "s" #'stfl/org-agenda-toggle-someday
      :desc "Add to Someday/Maybe" "S" #'stfl/org-agenda-set-someday
      :desc "Tickler toggle" "t" #'stfl/org-agenda-toggle-tickler
      :desc "Add to Tickler" "T" #'stfl/org-agenda-set-tickler
      :desc "Remove Someday/Maybe" "r" #'stfl/org-agenda-remove-someday

      :prefix ("v" . "View up to priority")
      "v" #'stfl/org-agenda-show-priorities
      "l" #'stfl/org-agenda-show-less-priorities
      "m" #'stfl/org-agenda-show-more-priorities
      "r" #'stfl/org-agenda-reset-show-priorities
      )

(map! :after org-ql
      :map org-ql-view-map
      "z" #'org-ql-view-dispatch)

;; (after! org
(setq!
       ;; org-agenda-dim-blocked-tasks t
       org-agenda-dim-blocked-tasks 'invisible
       org-agenda-use-time-grid t
       ;; org-agenda-hide-tags-regexp "\\w+"
       ;; org-agenda-compact-blocks t
       ;; org-agenda-block-separator ?\n
       org-agenda-block-separator ?-
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
       org-agenda-skip-scheduled-delay-if-deadline t
       org-agenda-skip-scheduled-if-deadline-is-shown t
       org-agenda-skip-timestamp-if-deadline-is-shown t
       ;; org-agenda-todo-ignore-with-date nil
       ;; org-agenda-todo-ignore-deadlines nil
       ;; org-agenda-todo-ignore-timestamp nil
       org-agenda-todo-list-sublevels t
       org-agenda-include-deadlines t
       org-stuck-projects '("-SOMEDAY/+PROJ" ("NEXT" "WAIT") ("WAITING") ""))

(setq stfl/org-agenda-primary-work-tags '("3datax" "@3datax" "#3datax"))

(after! org
  (setq org-enforce-todo-checkbox-dependencies nil
        org-enforce-todo-dependencies nil))

(setq stfl/proxmox-support-dir "~/Support/"
      stfl/org-gtd-inbox "inbox.org"
      stfl/org-gtd-inbox-orgzly "inbox-orgzly.org"
      stfl/org-gtd-inbox-absolute (doom-path org-directory stfl/org-gtd-inbox)
      stfl/org-gtd-todo "todo.org"
      stfl/org-gtd-todo-absolute (doom-path org-directory stfl/org-gtd-todo)
      ;; stfl/org-gtd-projects "gtd/projects/"
      stfl/org-gtd-projects '("emacs.org" "freelance.org"
                              "geschenke.org" "media.org" "projects.org"
                              "3datax.org" "pulswerk.org" "versicherung.org")
      stfl/org-roam-absolute (doom-path org-directory "roam/"))

(after! org
  (setq org-agenda-diary-file (doom-path org-directory "diary.org")
        org-agenda-files `(,stfl/org-gtd-inbox
                           ,stfl/org-gtd-inbox-orgzly
                           ,stfl/org-gtd-todo
                           ,@stfl/org-gtd-projects
                           ,@(file-expand-wildcards (doom-path stfl/proxmox-support-dir "**/*.org")))))

(after! org

(setq stfl/agenda-backlog-prio-threshold (+ 2 org-priority-default))

(setq-default stfl/agenda-max-prio-group ?D)
;; Priority level until the backlog in today-agenda is shown!

(setq stfl/agenda-deadline-fib-offset 3)

(setq org-agenda-custom-commands
      `(
        ;; ("a" "Private Agenda Today"
        ;;  (,(stfl/agenda-day)
        ;;   (org-ql-block (stfl/agenda-query-actions-prio-higher stfl/agenda-max-prio-group)
        ;;                 ((org-ql-block-header "Next Actions")
        ;;                  ;; (org-agenda-block-separator "\n")
        ;;                  ;; (org-super-agenda-header-separator "")
        ;;                  (org-super-agenda-groups stfl/ancestor-priority-groups)))
        ;;   (org-ql-block ((and (stuck-proj)
        ;;                       (private))
        ;;                  ((org-ql-block-header "Stuck Projects")
        ;;                   ;; (org-super-agenda-header-separator "")
        ;;                   (org-super-agenda-groups stfl/priority-groups)
        ;;                   )))))
        ("i" "Inbox"
         ((org-ql-block '(and (not (done))
                              (tags "#inbox" "inbox"))
                        ((org-ql-block-header "Inbox")
                         (org-super-agenda-groups '((:auto-property "CREATED")))))))
        ("a" "Private Agenda Today"
         (,(stfl/agenda-day)
          (org-ql-block `(and (todo "NEXT" "WAIT")
                              ,(prio-deadline>= stfl/agenda-max-prio-group)
                              (not ,(someday-habit))
                              (not (ancestors (deadline :to 0)))
                              (not (deadline :to 0))
                              (not (scheduled))
                              (not (primary-work)))
                        ((org-ql-block-header "Next Actions")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)))
          (org-ql-block '(and (stuck-proj)
                              (not (primary-work)))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-groups stfl/priority-groups)))))
        ("A" "Agenda Weekly"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)))))
        ("r" . "Review")
        ("rc" "Close open NEXT Actions and WAIT"
         ((org-ql-block '(and (todo "NEXT" "WAIT")
                              (not (tags "SOMEDAY" "HABIT" "org_jira"))
                              (not (my-habit))
                              (or (not (deadline))
                                  (deadline :to "+30")
                                  (ancestors (deadline :to "+30")))
                              (or (not (scheduled))
                                  (scheduled :to "+30")))
                        ((org-super-agenda-header-separator "")
                         (org-deadline-warning-days 30)
                         (stfl/agenda-max-prio-group org-priority-lowest)
                         (org-super-agenda-groups stfl/ancestor-priority-groups)
                         (org-ql-block-header "Something to do")))
          (org-ql-block (stfl/agenda-query-stuck-projects)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))))
        ("rl" "Agenda Weekly with Log"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-archives-mode t)
                   (org-agenda-start-with-log-mode '(closed))
                   (org-agenda-show-log t)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^.*DONE "))))))
        ("rs" "Stuck Projects"
         ((org-ql-block '(stuck-proj)
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))))
        ("rt" "Tangling TODOs"
         ((org-ql-block '(tangling)
                        ((org-ql-block-header "Tangling TODOs")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/priority-groups)))))
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
                                                    ))))))
        ("p" . "Private")
        ("pb" "Backlog"
         ((org-ql-block '(and (or (todo "PROJ")
                                  (standalone-next))
                              (not (primary-work))
                              (not (my-habit)))
                        ((org-ql-block-header "Backlog")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)
                         (org-dim-blocked-tasks t)))))
        ("ps" "Stuck Projects"
         (org-ql-block ((and (stuck-proj)
                             (not (primary-work)))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)))))
        ("w" . "Work")
        ("ww" "Work Agenda Primary"
         ((org-ql-block '(and (primary-work)
                              (not (done))
                              (or (my-habit)
                                  (deadline :to today)
                                  (scheduled :to today)
                                  (ts-active :on today)))
                        ((org-ql-block-header "Today")
                         (org-super-agenda-groups stfl/org-super-agenda-today-groups)))
          (org-ql-block `(and (todo "NEXT" "WAIT")
                              ,(prio-deadline>= org-priority-default)
                              (not ,(someday-habit))
                              (not (ancestors (deadline :to 0)))
                              (not (deadline :to 0))
                              (not (scheduled))
                              (primary-work))
                        ((org-ql-block-header "Next Actions")
                         (stfl/agenda-max-prio-group org-default-priority)
                         (org-super-agenda-groups stfl/ancestor-priority-groups)))
          (org-ql-block '(and (stuck-proj)
                              (primary-work))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)))))
        ("wa" "Work Agenda (not primary)"
         ((org-ql-block '(and (and (work) (not (primary-work)))
                              (not (done))
                              (or (my-habit)
                                  (deadline :to today)
                                  (scheduled :to today)
                                  (ts-active :on today)))
                        ((org-ql-block-header "Today")
                         (org-super-agenda-groups stfl/org-super-agenda-today-groups)))
          (org-ql-block `(and (todo "NEXT" "WAIT")
                              ,(prio-deadline>= org-priority-default)
                              (not ,(someday-habit))
                              (not (ancestors (deadline :to 0)))
                              (not (deadline :to 0))
                              (not (scheduled))
                              (and (work) (not (primary-work))))
                        ((org-ql-block-header "Next Actions")
                         (stfl/agenda-max-prio-group org-default-priority)
                         (org-super-agenda-groups stfl/ancestor-priority-groups)))
          (org-ql-block '(and (stuck-proj)
                              (and (work) (not (primary-work))))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)))))
        ("wb" "Proxmox Backlog"
         ((org-ql-block '(and (or (todo "PROJ")
                                  (standalone-next))
                              (primary-work))
                        ((org-ql-block-header "Backlog")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)
                         (org-dim-blocked-tasks t)))
          (org-ql-block '(and (stuck-proj)
                              (not (primary-work))
                              ((org-ql-block-header "Stuck Projects")
                               (org-super-agenda-header-separator "")
                               (org-super-agenda-groups stfl/ancestor-priority-groups))))))
        ;; ("wp" "Backlog Primary Work"
        ;;  ((org-ql-block '(and (or (todo "PROJ")
        ;;                           (standalone-next))
        ;;                       (primary-work))
        ;;                 ((org-ql-block-header "Backlog")
        ;;                  (org-super-agenda-groups stfl/ancestor-priority-groups)
        ;;                  (org-dim-blocked-tasks t)))))
        ("wB" "Backlog #work w/ 3DataX"
         ((org-ql-block '(and (or (todo "PROJ")
                                  (standalone-next))
                              (and (work)
                                   (not (primary-work))))
                        ((org-ql-block-header "Backlog")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)
                         (org-dim-blocked-tasks t)))))
        ("ws" "Stuck Projects"
         (org-ql-block ((and (stuck-proj)
                             (work))
                        ((org-ql-block-header "Stuck Projects")
                         (org-super-agenda-header-separator "")
                         (org-super-agenda-groups stfl/ancestor-priority-groups)))))
        ))

) ;; (after! org

(use-package! org-super-agenda
  :after (org-agenda evil-org-agenda)
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-separator "\n")

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
           ;; :deadline before  ;;TODO requires a date string https://github.com/alphapapa/org-super-agenda#normal-selectors
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

(after! org-super-agenda

(defun stfl/org-super-agenda-ancestor-priority-or-default<= (item prio)
  (org-with-point-at (org-find-text-property-in-string 'org-marker item)
    (<= (stfl/org-min-ancestor-priority-or-default) prio)))

(defun stfl/org-super-agenda-ancestor-priority<= (item prio)
  (org-with-point-at (org-find-text-property-in-string 'org-marker item)
    (<= (stfl/org-min-ancestor-priority) prio)))

;; (defun stfl/org-super-agenda-parent-PROJ-priority= (item prio)
;;   (org-with-point-at (org-find-text-property-in-string 'org-marker item)
;;     (<= (stfl/org-parent-PROJ-priority-or-adjusted-default) prio)))

(defun stfl/org-PROJ-priority<= (marker prio)
  (<= (stfl/org-parent-PROJ-priority-or-adjusted-default marker) prio))

(defun stfl/org-PROJ-priority= (marker prio)
  (let ((proj-prio (stfl/org-parent-PROJ-priority-or-adjusted-default marker)))
    (when proj-prio
      (= proj-prio prio))))

(defun stfl/org-parent-PROJ-priority-or-adjusted-default (marker)
  (org-with-point-at marker
    (stfl/org-at-point-parent-PROJ-priority-or-adjusted-default)))

(defun fib (n)
  (fib-iter 1 0 n))

(defun fib-iter (a b count)
  (if (= count 0)
      b
    (fib-iter (+ a b) a (- count 1))))

(setq stfl/ancestor-priority-groups
      (append
       `((:name "Tickler"
          :and (:scheduled t
                :tag "SOMEDAY")
          :order ,(+ 1 org-priority-lowest)))      ;; and order in the appropriate position
       `((:name "Someday"
          :tag "SOMEDAY"
          :order ,(+ 2 org-priority-lowest)))      ;; and order in the appropriate position
       `,(mapcar
          (lambda (prio)
            (let ((prio-str (char-to-string prio))
                  (until-date-str
                   (ts-format "%Y-%m-%d"
                              (ts-adjust 'day
                                         (fib (+ stfl/agenda-deadline-fib-offset (- prio 64)))
                                         (ts-now)))))
              `(:name ,(format "[#%s] Priority %s" prio-str prio-str)
                :deadline (before ,until-date-str)
                :scheduled (before ,until-date-str)
                :priority ,prio-str
                :pred ((lambda (item)
                         (stfl/org-PROJ-priority=
                          (org-find-text-property-in-string 'org-marker item)
                          ,prio)))
                ;; :pred ((lambda (item))) TODO (stfl/org-PROJ-deadline-before (org-find-text-property-in-string 'org-marker item)
                ;;              (ts-format "%Y-%m-%d" (ts-adjust 'day (fib (+ stfl/agenda-deadline-fib-offset (- prio 64))) (ts-now)))
                :order ,prio)))
          (number-sequence org-priority-highest org-priority-lowest))
       `((:name "Default Priority (Rest)"
          :anything t                                ;; catch the rest
          :order ,(+ 0.5 org-priority-default)))      ;; and order in the appropriate position
       ))

(defun stfl/org-min-ancestor-priority-or-default ()
  (cl-loop minimize (save-match-data (stfl/org-priority-or-default))
           while (and (not (equal "PROJ" (nth 2 (org-heading-components))))
                      (org-up-heading-safe))))

(defun stfl/org-min-ancestor-priority-or-lowest ()
  (cl-loop minimize (save-match-data (stfl/org-priority-or-lowest))
           while (and (not (equal "PROJ" (nth 2 (org-heading-components))))
                      (org-up-heading-safe))))

(defun stfl/org-priority-or-lowest ()
  (let* ((prio-raw (org-element-property :priority (org-element-at-point)))
         (prio (cond (prio-raw prio-raw)
                     (t org-priority-lowest)))) ;; display empty prio below default
    prio))

(defun stfl/org-at-point-parent-PROJ-priority-or-adjusted-default ()
  (cl-loop minimize (when (equal "PROJ" (nth 2 (org-heading-components)))
                      (stfl/org-priority-or-default))
           while (and (not (equal "PROJ" (nth 2 (org-heading-components))))
                      (org-up-heading-safe))))

)

(setq stfl/org-super-agenda-today-groups
      '((:time-grid t
               :order 0)
        (:name "Tickler"
               :tag "SOMEDAY"
               :order 20)
        (:name "Habits"
               :tag "HABIT"
               :habit t
               :order 90)
        (:name "Today"
               :anything t
               :order 10)))

(setq stfl/org-super-agenda-today-groups-no-primary-work
      (let ((discard-primary `(:discard (:name "Primary Work"
                                         :tag ,stfl/org-agenda-primary-work-tags
                                         :order 40))))
        (cons discard-primary stfl/org-super-agenda-today-groups)))

(defun stfl/org-agenda-set-someday (&optional do-schedule)
  "Marks the current agenda entry as SOMEDAY

When called with the universial prefix `C-u` asks for a date on which it will be
relevant again"
  (interactive "P")
  (org-agenda-set-tags "SOMEDAY" 'on)
  (ignore-error user-error
    (org-agenda-priority 'remove))
  (org-agenda-deadline '(4))
  (org-agenda-schedule (unless do-schedule '(4))))

(defun stfl/org-agenda-set-tickler ()
  "Marks the current agenda entry as SOMEDAY and assign a scheduled date"
  (interactive)
  (stfl/org-agenda-set-someday '(4)))

(defun stfl/org-agenda-remove-someday ()
  "Remove SOMEDAY tag and scheduling from the current element and reintegrate into the Agenda"
  (interactive)
  (unless (stfl/org-agenda-someday?)
    (error "Element has no SOMEDAY tag"))
  (org-agenda-set-tags "SOMEDAY" 'off)
  (ignore-error user-error
    (org-agenda-priority 'remove))
  (org-agenda-deadline '(4))
  (org-agenda-schedule '(4)))

(defun stfl/org-agenda-someday? ()
  (-find (-partial 'string= "SOMEDAY") (org-get-at-bol 'tags)))

(defun stfl/org-agenda-toggle-someday (&optional do-schedule)
  "Toggle the SOMEDAY status

When called with the universial prefix `C-u` asks for a date on which it will be
relevant again (Tickler)"
  (interactive "P")
  (if (stfl/org-agenda-someday?)
      (stfl/org-agenda-remove-someday)
    (stfl/org-agenda-set-someday (when do-schedule '(4)))))

(defun stfl/org-agenda-toggle-tickler ()
  "Toggle SOMEDAY status and ask for a date when to put on the tickler"
  (interactive)
  (stfl/org-agenda-toggle-someday '(4)))

(defun stfl/agenda-query-stuck-projects()
  '(stuck-proj))

(defun stfl/org-agenda-show-priorities (&optional priority)
  (interactive "P")
  (setq-local new (cond ((equal priority '(4)) stfl/agenda-max-prio-group)
                        (priority)
                        (t (upcase (read-char (format "Show up to priority (%c-%c): " org-priority-highest org-priority-lowest))))))
  (when (or (< new org-priority-highest) (> new org-priority-highest))
    (user-error "Priority must be between org-priority-highest and org-priority-lowest"))
  (setq stfl/agenda-max-prio-group new)
  (message "Showing up to priority %c" new)
  (org-agenda-redo-all))

(defun stfl/org-agenda-reset-show-priorities ()
  (interactive)
  (setq stfl/agenda-max-prio-group (default-value 'stfl/agenda-max-prio-group))
  (org-agenda-redo-all))

(defun stfl/org-agenda-show-more-priorities ()
  (interactive)
  (setq stfl/agenda-max-prio-group (min (1+ stfl/agenda-max-prio-group) org-priority-lowest))
  (org-agenda-redo-all))

(defun stfl/org-agenda-show-less-priorities ()
  (interactive)
  (setq stfl/agenda-max-prio-group (max (1- stfl/agenda-max-prio-group) org-priority-highest))
  (org-agenda-redo-all))

(defun stfl/agenda-day ()
  '(agenda "Agenda"
           ((org-agenda-use-time-grid t)
            (org-deadline-warning-days 0)
            (org-agenda-span '1)
            (org-super-agenda-groups stfl/org-super-agenda-today-groups-no-primary-work)
            (org-agenda-start-day (org-today)))))

(defun prio-deadline>= (prio)
  `(and (or (priority >= (char-to-string ,prio))
            (and ,(> stfl/agenda-max-prio-group org-priority-default)
                 (not (priority)))  ;; default priority is treated as nil in org-ql
            (ancestors (priority >= (char-to-string ,prio)))
            (deadline :to ,(1-          ;; decrease by 1 to match the org-super-agenda (deadline (before X)) behaviour
                            (fib        ;; increase the date range of interest with a fibonacci sequance
                             (+ stfl/agenda-deadline-fib-offset              ;; start the sequeance at (fib 4)
                                (- prio 64))))) ;; use the priority value
            (ancestors (deadline :to ,(1- (fib (+ stfl/agenda-deadline-fib-offset
                                                   (- prio 64)))))))))

(defun stfl/agenda-query-actions-prio-higher (prio)
  `(and (todo "NEXT" "WAIT")
        ,(prio-deadline>= prio)
        (not ,(someday-habit))
        (not (ancestors (deadline :to 0)))
        (not (deadline :to 0))
        (not (scheduled))))

(defun someday-habit()
  '(or (tags "SOMEDAY" "HABIT")
        (habit)))

(defun not-someday-habit()
  `(not ,(someday-habit)))

(defun not-sched-or-dead(from)
  `(and (not (scheduled :from today))
       (not (deadline :from ,from))))

(defun stfl/org-ql-min-ancestor-priority< (a b)
  "Return non-nil if A's minimum ancestor priority is higher than B's.
A and B are Org headline elements.
org-default-priority is treated as lower than the same set value"
  (cl-macrolet ((priority (item)
                          `(org-with-point-at (org-element-property :org-marker ,item)
                             (stfl/org-min-ancestor-priority))))
    ;; NOTE: Priorities are numbers in Org elements.
    ;; This might differ from the priority selector logic.
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
                     (t (+ 0.5 org-priority-default))))) ;; display empty prio below default
    prio))

(after! org-ql
  (org-ql-defpred tickler ()
    "match entries in the \"tickler\"."
    :normalizers ((`(,predicate-names)
                   (rec '(and (todo) (tags-local "SOMEDAY") (scheduled)))))
    :preambles ((`(,predicate-names)
                 (rec '(and (todo) (tags-local "SOMEDAY") (scheduled))))))

  (org-ql-defpred tickler-proj ()
    "match PROJ in the \"tickler\" or all children in \"tickler\"."
    :normalizers ((`(,predicate-names)
                   (rec '(and (todo "PROJ")
                              (or (tickler)
                                  (and (children (tickler))
                                       (not (children (and (todo "NEXT" "WAIT")
                                                           (not (tickler)))))))))))
    :preambles ((`(,predicate-names)
                 (rec '(and (todo "PROJ")
                            (or (tickler)
                                (and (children (tickler))
                                     (not (children (and (todo "NEXT" "WAIT")
                                                         (not (tickler))))))))))))

  (org-ql-defpred work ()
    "work related entries."
    :normalizers ((`(,predicate-names)
                   (rec '(tags "#work"))))
    :preambles ((`(,predicate-names)
                 (rec '(tags "#work")))))

  (org-ql-defpred primary-work ()
    "work related entries."
    :normalizers ((`(,predicate-names)
                   (rec `(tags ,@stfl/org-agenda-primary-work-tags))))
    :preambles ((`(,predicate-names)
                 (rec `(tags ,@stfl/org-agenda-primary-work-tags)))))

  (org-ql-defpred private ()
    "Private entries."
    :normalizers ((`(,predicate-names)
                   (rec '(not (tags "#work")))))
    :preambles ((`(,predicate-names)
                   (rec '(not (tags "#work"))))))

  (org-ql-defpred (stuck-proj stuck) ()
    "Stuck Project"
    :normalizers ((`(,predicate-names)
                   (rec '(and (todo "PROJ")
                              (not (tags "SOMEDAY"))
                              (not (children (todo "NEXT" "WAIT")))
                              (not (tickler-proj))))))
    :preambles ((`(,predicate-names)
                 (rec '(and (todo "PROJ")
                            (not (tags "SOMEDAY"))
                            (not (children (todo "NEXT" "WAIT")))
                            (not (tickler-proj)))))))


  (org-ql-defpred standalone-next ()
    "Standalone NEXT Action (or WAIT)"
    :normalizers ((`(,predicate-names)
                   (rec '(and (todo "NEXT" "WAIT")
                              (not (ancestors (or (todo "PROJ")
                                                  (done))))))))
    :preambles ((`(,predicate-names)
                 (rec '(and (todo "NEXT" "WAIT")
                              (not (ancestors (or (todo "PROJ")
                                                  (done)))))))))

  (org-ql-defpred tangling ()
    "Tangling Actions (Ancestors Done)"
    :normalizers ((`(,predicate-names)
                   (rec '(and (todo) (ancestors (done))))))
    :preambles ((`(,predicate-names)
                   (rec '(and (todo) (ancestors (done)))))))

  (org-ql-defpred someday ()
    "tagged SOMEDAY"
    :normalizers ((`(,predicate-names)
                   (rec '(tags "SOMEDAY"))))
    :preambles ((`(,predicate-names)
                 (rec '(tags "SOMEDAY")))))

  (org-ql-defpred my-habit ()
    "style habit or tag HABIT"
    :normalizers ((`(,predicate-names)
                   (rec '(or (tags "HABIT") (habit)))))
    :preambles ((`(,predicate-names)
                 (rec '(or (tags "HABIT") (habit))))))

;; (defun prio-deadline>= (prio)
;;   `(and (or (priority >= (char-to-string ,prio))
;;             (and ,(> stfl/agenda-max-prio-group org-priority-default)
;;                  (not (priority)))  ;; default priority is treated as nil in org-ql
;;             (ancestors (priority >= (char-to-string ,prio)))
;;             (deadline :to ,(1-          ;; decrease by 1 to match the org-super-agenda (deadline (before X)) behaviour
;;                             (fib        ;; increase the date range of interest with a fibonacci sequance
;;                              (+ stfl/agenda-deadline-fib-offset              ;; start the sequeance at (fib 4)
;;                                 (- prio 64)))) ;; use the priority value
;;                       )
;;             (ancestors (deadline :to ,(1- (fib (+ stfl/agenda-deadline-fib-offset
;;                                                    (- prio 64)))))))))



;;   (org-ql-defpred prio-deadline ()
;;     "Priority defined by priority, deadline of entry or ancestors."
;;     (let ((deadline-limit
;;            (1-          ;; decrease by 1 to match the org-super-agenda (deadline (before X)) behaviour
;;                             (fib        ;; increase the date range of interest with a fibonacci sequance
;;                              (+ stfl/agenda-deadline-fib-offset              ;; start the sequeance at (fib 4)
;;                                 (- prio 64)))) ;; use the priority value
;;            ))
;;     :normalizers ((`(,predicate-names)
;;                    (rec '

;;                     (and (todo) (tags-local "SOMEDAY") (scheduled))


;;                 )))
;;     :preambles ((`(,predicate-names)
;;                  (rec '(and (todo) (tags-local "SOMEDAY") (scheduled))))))

)

(after! org-ql
  (setq org-ql-views
         (list (cons "LASTMILE"
                     (list :title "LASTMILE"
                           :buffers-files 'org-agenda-files
                           :sort 'priority
                           :super-groups #'stfl/ancestor-priority-groups
                           :query `(and
                                    (todo "NEXT")
                                    (tags "LASTMILE")
                                    ,(not-someday-habit)
                                    ,(not-sched-or-dead 14))
                           :narrow nil))
               (cons "PROJ Backlock Active"
                     (list :title "PROJ Backlog"
                           :buffers-files 'org-agenda-files
                           :sort 'priority
                           :super-groups #'stfl/ancestor-priority-groups
                           :query `(and (todo "PROJ")
                                        ,(not-someday-habit)
                                        (not (tickler-proj)))
                           :narrow nil))
               (cons "PROJ Backlock #work"
                     (list :title "#work backlog with tickler"
                           :buffers-files 'org-agenda-files
                           :sort 'priority
                           :super-groups #'stfl/ancestor-priority-groups
                           :query `(and (or (todo "PROJ")
                                            (standalone-next))
                                        (tags "#work")
                                        ,(not-someday-habit)
                                        (not (tickler-proj)))
                           :narrow nil))
               (cons "PROJ Backlock #work (full)"
                     (list :title "#work backlog with tickler"
                           :buffers-files 'org-agenda-files
                           :sort 'priority
                           :super-groups #'stfl/ancestor-priority-groups
                           :query `(and (or (todo "PROJ")
                                            (standalone-next))
                                        (tags "#work"))
                           :narrow nil))

               ;;     ("Home and Sarah"
               ;;     :title "Home and Sarah"
               ;;     :buffers-files org-agenda-files
               ;;     :sort priority
               ;;     :super-groups stfl/ancestor-priority-groups
               ;;     :query `(and
               ;;             (todo "NEXT" "NEXT")
               ;;             (tags "@sarah" "@home")
               ;;             ,(not-someday-habit)
               ;;             ,(not-sched-or-dead 14))
               ;;     :narrow nil)
               ;; ("Standalong NEXT"
               ;;     :title "Standalone NEXT"
               ;;     :buffers-files org-agenda-files
               ;;     :sort priority
               ;;     :super-groups stfl/ancestor-priority-groups
               ;;     :query `(and
               ;;             (todo "NEXT" "NEXT")
               ;;             (not (ancestors (todo "PROJ")))
               ;;             ,(not-someday-habit)
               ;;             ,(prio-deadline>= org-priority-lowest)
               ;;     :narrow nil))
               ;; ("Open Loops"
               ;;     :title "Open Loops"
               ;;     :buffers-files org-agenda-files
               ;;     :sort priority
               ;;     :super-groups stfl/ancestor-priority-groups
               ;;     :query `(and
               ;;             (todo "NEXT" "NEXT")
               ;;             ;; (not (ancestors (todo "PROJ")))
               ;;             ,(not-someday-habit)
               ;;             ,(prio-deadline>= org-priority-lowest)
               ;;     :narrow nil))
               ;; ("Stuck Standalone NEXT and WAIT"
               ;;     :title "Stuck Standalong NEXT"
               ;;     :buffers-files org-agenda-files
               ;;     :sort priority
               ;;     :super-groups stfl/ancestor-priority-groups
               ;;     :query `(and
               ;;             (todo "NEXT" "WAIT")
               ;;             (not (ancestors (todo "PROJ")))
               ;;             (not (scheduled))
               ;;             (not (deadline))
               ;;             (not (tags "@crypto_rotation" "inbox"))
               ;;             ,(not-someday-habit)
               ;;             (not ,(stfl/agenda-query-actions-prio-higher stfl/agenda-max-prio-group))
               ;;     :narrow nil))
               ;; ("Crypo Rotation"
               ;;     :title "Crypto Rotation"
               ;;     :buffers-files org-agenda-files
               ;;     :sort date
               ;;     :super-groups stfl/ancestor-priority-groups
               ;;     :query `(and
               ;;             (todo "NEXT" "TODO")
               ;;             (ts-active :to today)
               ;;             (tags "@crypto_rotation")
               ;;             ,(not-someday-habit)
               ;;             (not ,(stfl/agenda-query-actions-prio-higher stfl/agenda-max-prio-group))
               ;;     :narrow nil))

               ;; ("Calendar: This week"
               ;;       (lambda ()
               ;;         "Show items with an active timestamp during this calendar week."
               ;;         (interactive)
               ;;         (let* ((ts (ts-now))
               ;;                (beg-of-week (->> ts
               ;;                               (ts-adjust 'day (- (ts-dow (ts-now))))
               ;;                               (ts-apply :hour 0 :minute 0 :second 0)))
               ;;                (end-of-week (->> ts
               ;;                               (ts-adjust 'day (- 6 (ts-dow (ts-now))))
               ;;                               (ts-apply :hour 23 :minute 59 :second 59))))
               ;;           (org-ql-search (org-agenda-files)
               ;;             `(ts-active :from ,beg-of-week
               ;;                         :to ,end-of-week)
               ;;             :title "This week"
               ;;             :super-groups 'org-super-agenda-groups
               ;;             :sort '(priority)))))
               ;; ("Calendar: Next week"
               ;;       (lambda ()
               ;;         "Show items with an active timestamp during the next calendar week."
               ;;         (interactive)
               ;;         (let* ((ts (ts-adjust 'day 7 (ts-now)))
               ;;                (beg-of-week (->> ts
               ;;                               (ts-adjust 'day (- (ts-dow (ts-now))))
               ;;                               (ts-apply :hour 0 :minute 0 :second 0)))
               ;;                (end-of-week (->> ts
               ;;                               (ts-adjust 'day (- 6 (ts-dow (ts-now))))
               ;;                               (ts-apply :hour 23 :minute 59 :second 59))))
               ;;           (org-ql-search (org-agenda-files)
               ;;             `(ts-active :from ,beg-of-week
               ;;                         :to ,end-of-week)
               ;;             :title "Next week"
               ;;             :super-groups 'org-super-agenda-groups
               ;;             :sort '(priority)))))

               ;; ("Review: Recently timestamped" 'org-ql-view-recent-items)
               ;; ((propertize "Review: Dangling tasks"
               ;;                   'help-echo "Tasks whose ancestor is done")
               ;;       (list :buffers-files #'org-agenda-files
               ;;             :query '(and (todo)
               ;;                          (ancestors (done)))
               ;;             :title (propertize "Review: Dangling tasks"
               ;;                                'help-echo "Tasks whose ancestor is done")
               ;;             :sort '(todo priority date)
               ;;             :super-groups '((:auto-parent t))))
               ;; ((propertize "Review: Stale tasks"
               ;;                   'help-echo "Tasks without a timestamp in the past 2 weeks")
               ;;       (list :buffers-files #'org-agenda-files
               ;;             :query '(and (todo)
               ;;                          (not (ts :from -14)))
               ;;             :title (propertize "Review: Stale tasks"
               ;;                                'help-echo "Tasks without a timestamp in the past 2 weeks")
               ;;             :sort '(todo priority date)
               ;;             :super-groups '((:auto-parent t))))
               ;; (,(propertize "Review: Stuck projects"
               ;;                   'help-echo "Tasks with sub-tasks but no NEXT sub-tasks")
               ;;       (list :buffers-files #'org-agenda-files
               ;;             :query (and (todo)
               ;;                          (descendants (todo))
               ;;                          (not (descendants (todo "NEXT"))))
               ;;             :title (propertize "Review: Stuck projects"
               ;;                                'help-echo "Tasks with sub-tasks but no NEXT sub-tasks")
               ;;             :sort (date priority)
               ;;             :super-groups 'org-super-agenda-groups))
               ))
  )

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
;  (setq projectile-files-cache-expire 30)
)

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

(after! vterm
  (setq! vterm-max-scrollback 200000))

(map! :after vterm :map vterm-mode-map "C-c C-x" #'vterm--self-insert)

(use-package! typst-ts-mode
  :init
  (setq! typst-ts-watch-options "--open"
         typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory)
         typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (map! :map typst-ts-mode-map
        "C-c C-c" #'typst-ts-tmenu)
  (add-hook! 'typst-ts-mode-hook #'lsp-deferred))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst") t)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "typst-lsp")
                    :activation-fn (lsp-activate-on "typst")
                    :server-id 'typst-lsp)))

(after! org
  (add-to-list 'org-src-lang-modes '("typst" . typst-ts))
  
  ;; Set up babel support for Typst
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typst . t)))

  ;; Configure babel execution for Typst
  (defun org-babel-execute:typst (body params)
    "Execute a block of Typst code with org-babel."
    (message "Executing Typst code block")
    (let* ((in-file (org-babel-temp-file "typst-" ".typ"))
           (out-file (or (cdr (assq :file params))
                         (org-babel-temp-file "typst-" ".pdf"))))
      (with-temp-file in-file
        (insert body))
      (org-babel-eval
       (format "typst compile %s %s" in-file out-file)
       "")
      nil))) ; Return nil as we're not inserting results into the org buffer

;; If you want to add a structure template for Typst blocks
;; TODO working? expansion not working?
(after! org-tempo
  (add-to-list 'org-structure-template-alist '("y" . "src typst")))

(map! :leader ":" #'ielm)



(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :after prog-mode)

(after! (evil copilot)
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun +copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.
  
  ;; Bind the custom function to <tab> in Evil's insert state
  (evil-define-key 'insert 'global (kbd "<tab>") #'+copilot-tab-or-default)

  (map! :map copilot-completion-map
        :i "<tab>" #'+copilot-tab-or-default
        :i "TAB" #'+copilot-tab-or-default
        ;; :i "C-TAB" #'copilot-accept-completion-by-word
        ;; :i "C-<tab>" #'copilot-accept-completion-by-word
        :i "C-S-n" #'copilot-next-completion
        ;; :i "C-<tab>" #'copilot-next-completion
        :i "C-S-p" #'copilot-previouse-completion
        ;; :i "C-<iso-lefttab>" #'copilot-previouse-completion
        ))

(use-package! codeium
  :disabled

  :defer t  ;; TODO to start it, manually call codeium-init

  ;; if you use straight
  ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
  ;; otherwise, make sure that the codeium.el file is on load-path

  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  ;; (add-to-list 'company-frontends #'company-preview-frontend)
  (setq company-minimum-prefix-length 0)

  ;; or on a hook
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

  ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions
  ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))

  ;; TODO for completion at point to work need to add codeium-completion-at-point to completion-at-point-an

  ;; functions async company-backend is coming soon!

  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  ;; (add-hook 'emacs-startup-hook
  ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  :config
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  (setq codeium/metadata/api_key (get-password :host "Codeium"))

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)

  (let ((codeium-exe (executable-find "codeium_language_server")))
    (when codeium-exe
      (setq codeium-command-executable codeium-exe)))
  )

(use-package! lsp-treemacs
  :after lsp-mode  ;; and treemacs
  :config (lsp-treemacs-sync-mode 1))

(map! (:after lsp
       :map lsp-mode-map
       :leader
       :prefix ("c" . "+code")
       :desc "Diagnostic for Workspace" "X" #'lsp-treemacs-errors-list))

(after! lsp-mode
  (setq! lsp-inlay-hint-enable t
         lsp-headerline-breadcrumb-enable t))

(when (< emacs-major-version 30)
  (after! lsp-mode
    (when (executable-find "emacs-lsp-booster")
      (defun lsp-booster--advice-json-parse (old-fn &rest args)
        "Try to parse bytecode instead of json."
        (or
         (when (equal (following-char) ?#)
           (let ((bytecode (read (current-buffer))))
             (when (byte-code-function-p bytecode)
               (funcall bytecode))))
         (apply old-fn args)))
      (advice-add (if (progn (require 'json)
                             (fboundp 'json-parse-buffer))
                      'json-parse-buffer
                    'json-read)
                  :around
                  #'lsp-booster--advice-json-parse)

      (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
        "Prepend emacs-lsp-booster command to lsp CMD."
        (let ((orig-result (funcall old-fn cmd test?)))
          (if (and (not test?)                             ;; for check lsp-server-present?
                   (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                   lsp-use-plists
                   (not (functionp 'json-rpc-connection))  ;; native json-rpc
                   (executable-find "emacs-lsp-booster"))
              (progn
                (message "Using emacs-lsp-booster for %s!" orig-result)
                (cons "emacs-lsp-booster" orig-result))
            orig-result)))
      (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))
  )

(map! (:when (modulep! :editor format)
       :v "g Q" '+format/region
       :v "SPC =" '+format/region
       :leader
       :desc "Format Buffer" "=" #'+format/buffer
       (:prefix ("b" "+buffer")
        :desc "Format Buffer" "f" #'+format/buffer)))

(after! (lsp-mode php-mode)
  (setq lsp-intelephense-licence-key (get-auth-info "intelephense" "ste.lendl@gmail.com")
        lsp-intelephense-files-associations '["*.php" "*.phtml" "*.inc"]
        lsp-intelephense-files-exclude '["**update.php**" "**/js/**" "**/fonts/**" "**/gui/**" "**/upload/**"
                                         "**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**"
                                         "**/node_modules/**" "**/bower_components/**"
                                         "**/vendor/**/{Test,test,Tests,tests}/**"]
        lsp-auto-guess-root nil
        lsp-idle-delay 0.8))

(after! poetry (setq poetry-tracking-strategy 'projectile))

(after! conda (conda-env-autoactivate-mode))

(after! projectile
  (projectile-register-project-type 'python-conda '("environment.yml")
                                    :project-file "environment.yml"
                                    :compile "conda build"  ;; does not exist
                                    :test "conda run pytest"
                                    :test-dir "tests"
                                    :test-prefix "test_"
                                    :test-suffix"_test"))

;; (use-package! numpydoc
;;   :after python-mode
;;   :commands numpydoc-generate
;;   :config
;;   (map! :map python-mode-map
;;         :localleader
;;         :prefix ("d" . "docstring")
;;         :desc "Generate Docstring" "d" #'numpydoc-generate))

(after! ein
  (setq! ein:output-area-inlined-images t
         ein:worksheet-warn-obsolesced-keybinding nil))

(when (modulep! :tools ein)
  (after! org
    (require 'ob-ein)))

(after! org
  (setq org-babel-default-header-args:jupyter-python
        '((:results . "value")
          (:session . "jupyter")
          (:kernel . "python3")
          (:pandoc . "t")
          (:exports . "both")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:eval . "never-export"))))

(after! (python-mode dap-mode)
  (dap-register-debug-template "Python :: Run pytest (at point) -- Workaround"
                             (list :type "python-test-at-point  "
                                   :args ""
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: Run pytest (at point)")))

(after! lsp-rust
  (setq! lsp-rust-analyzer-binding-mode-hints t
         lsp-rust-analyzer-display-chaining-hints t
         lsp-rust-analyzer-display-closure-return-type-hints t
         lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
         lsp-rust-analyzer-display-parameter-hints t
         lsp-rust-analyzer-hide-named-constructor t))

(after! (rust-mode dap-mode)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

(after! nix-mode
  (set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode)))

(setq-hook! 'nix-mode-hook +format-with-lsp nil)

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

(use-package! exercism-mode
  :after projectile
  :if (executable-find "exercism")
  :commands exercism
  :config (exercism-mode +1)
  :custom (exercism-web-browser-function 'browse-url))

(map! :after rjsx-mode
      :map rjsx-mode-map
      :localleader
      :prefix ("t" "test")
      "f" #'jest-file
      "t" #'jest-function
      "k" #'jest-file-dwim
      "m" #'jest-repeat
      "p" #'jest-popup)

(add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))

(use-package! logview
  :commands logview-mode
  :config (setq truncate-lines t)
  (map! :map logview-mode-map
        "j" #'logview-next-entry
        "k" #'logview-previous-entry))

;; (add-to-list 'lsp-ltex-active-modes 'adoc-mode t)
(setq lsp-ltex-active-modes '(text-mode bibtex-mode context-mode latex-mode markdown-mode org-mode rst-mode adoc-mode))

(use-package! lsp-ltex
  :after ;; (lsp-mode adoc-mode)
        lsp-ltex-active-modes
  :hook (adoc-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-server-store-path "~/.nix-profile/bin/ltex-ls"
        lsp-ltex-version "16.0.0"
        lsp-ltex-mother-tongue "de-AT"
        lsp-ltex-user-rules-path (doom-path doom-private-dir "lsp-ltex")))

(use-package! ssh-config-mode :defer t)

;; (use-package! bitbake-modes
;;   :config (add-to-list 'auto-mode-alist '("\\.inc$" . bitbake-mode))
;;   :defer t)

(use-package! meson-mode
  :disabled
  :config (add-hook! 'meson-mode-hook #'company-mode))

(defun run-ctest (arg)
  (interactive "P")
  (let ((projectile-project-test-cmd "cmake --build build && ctest --test-dir build --output-on-failure --rerun-failed"))
    (projectile-test-project arg)))

(map! :after c++-mode
      :map c++-mode-map
      :localleader 
      :prefix ("t" "test")
      :n "t" #'run-ctest
      ;; :n "t" #'gtest-run-at-point
      ;; :n "T" #'gtest-run
      ;; :n "l" #'gtest-list
      )

(after! lsp-mode
  (set-lsp-priority! 'ccls 2))

;; (use-package! gtest-mode
;;   ;; :after c++-mode
;;   :config
;;   (map! :map gtest-mode-map
;;         :localleader 
;;         :prefix ("t" "test")
;;         :n "t" #'gtest-run-at-point
;;         :n "T" #'gtest-run
;;         :n "l" #'gtest-list))

(use-package! turbo-log
  :after prog-mode
  ;; :bind (("C-s-l" . turbo-log-print)
  ;;        ("C-s-i" . turbo-log-print-immediately)
  ;;        ("C-s-h" . turbo-log-comment-all-logs)
  ;;        ("C-s-s" . turbo-log-uncomment-all-logs)
  ;;        ("C-s-[" . turbo-log-paste-as-logger)
  ;;        ("C-s-]" . turbo-log-paste-as-logger-immediately)
  ;;        ("C-s-x" . turbo-log-delete-all-logs))
  :config
  (map! :leader
        "l l" #'turbo-log-print
        "l i" #'turbo-log-print-immediately
        "l h" #'turbo-log-comment-all-logs
        "l s" #'turbo-log-uncomment-all-logs
        "l [" #'turbo-log-paste-as-logger
        "l ]" #'turbo-log-paste-as-logger-immediately
        "l x" #'turbo-log-delete-all-logs)
  (setq turbo-log-msg-format-template "\"🚀: %s\""
        turbo-log-allow-insert-without-tree-sitter-p t))

(use-package! ztree)

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(after! magit
  (setq magit-diff-refine-hunk 'all))

(after! forge (setq forge-topic-list-columns
                    '(("#" 5 t (:right-align t) number nil)
                      ("Title" 60 t nil title  nil)
                      ("State" 6 t nil state nil)
                      ("Marks" 8 t nil marks nil)
                      ("Labels" 8 t nil labels nil)
                      ("Assignees" 10 t nil assignees nil)
                      ("Updated" 10 t nill updated nil))))

(setq! magit-todos-exclude-globs '(".git/" "node_modules/"))

;; (after! magit-todos (magit-todos-mode))

;; (set-email-account! "gmail"
;;   '((mu4e-sent-folder       . "/gmail/[Google Mail]/Gesendet")
;;     (mu4e-drafts-folder     . "/gmail/[Google Mail]/Entw&APw-rfe")
;;     (mu4e-trash-folder      . "/gmail/[Google Mail]/Trash")
;;     (mu4e-refile-folder     . "/gmail/[Google Mail]/Alle Nachrichten")
;;     (smtpmail-smtp-user     . "ste.lendl@gmail.com")
;;     ;; (+mu4e-personal-addresses . "ste.lendl@gmail.com")
;;     ;; (mu4e-compose-signature . "---\nStefan Lendl")
;;     )
;;   t)

;; (set-email-account! "pulswerk"
;;   '((mu4e-sent-folder       . "/pulswerk/Sent Items")
;;     (mu4e-drafts-folder     . "/pulswerk/Drafts")
;;     (mu4e-trash-folder      . "/pulswerk/Deleted Items")
;;     (mu4e-refile-folder     . "/pulswerk/Archive")
;;     (smtpmail-smtp-user     . "lendl@pulswerk.at")
;;     ;; (+mu4e-personal-addresses . "lendl@pulswerk.at")
;;     ;; (mu4e-compose-signature . "---\nStefan Lendl")
;;     )
;;   t)

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

(use-package! diffview
  :commands diffview-current
  :config
  (map!
   :after notmuch
   :localleader "d" #'diffview-current))

(use-package! blamer
  :commands global-blamer-mode
  :init (map! :leader "t B" #'global-blamer-mode)
  :config
  (map! :leader "g i" #'blamer-show-posframe-commit-info)
  (setq! blamer-idle-time 0.3
         blamer-type 'visual
         blamer-max-commit-message-length 80
         ;; blamer-max-lines 100
         ;; blamer-type 'posframe-popup
         ;; blamer-type 'overlay-popup
         blamer-min-offset 40)
  ;; (add-hook! org-mode-hook (λ! (blamer-mode 0)))
  )

(use-package! gptel
  :after auth-source
  :commands gptel
  :config
  (setq! gptel-default-mode 'org-mode
                                        ;         gptel-response-prefix-alist '((org-mode . "**** Answer"))
         gptel-api-key (get-password :host "OpenAI-gptel")
         gptel-model "gpt-4o"
         gptel-log-level 'info
         gptel-use-curl t
         gptel-stream t)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  ;; TODO add org-babel reload or sth to the reponse hook
  ;; (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  (set-popup-rule! "^\\*ChatGPT\\*$" :select t
    :quit nil :ttl nil :modeline t :persist t)
  
  (gptel-make-anthropic "Claude"          ;Any name you want
    :stream t                             ;Streaming responses
    :key (get-password :host "Claude-gptel")))
