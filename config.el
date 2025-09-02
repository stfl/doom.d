(setq user-full-name "Stefan Lendl"
      user-mail-address "ste.lendl@gmail.com")

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(defun stfl/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.org" doom-user-dir)))

(define-key! help-map
      "dc" #'stfl/goto-private-config-file
      "dC" #'doom/open-private-config)

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

(after! evil-escape
  (setq evil-escape-key-sequence "jk"))

(use-package! drag-stuff
  :defer t
  :init
  (map! "<M-up>"    #'drag-stuff-up
        "<M-down>"  #'drag-stuff-down
        "<M-left>"  #'drag-stuff-left
        "<M-right>" #'drag-stuff-right))

(setq gc-cons-threshold most-positive-fixnum)

(run-with-idle-timer 1.2 t 'garbage-collect)

(setq! focus-follows-mouse 'auto-raise
       mouse-autoselect-window nil)

(setq doom-theme 'doom-one)

(setq! display-line-numbers-type t)
(setq! which-key-idle-delay 0.3)

(let ((font "JetBrains Mono Nerd Font Mono"))
  (setq doom-font (font-spec :family font :size 13)
        doom-variable-pitch-font (font-spec :family font)
        doom-big-font (font-spec :family font :size 20)))

(custom-set-faces!
  `(whitespace-indentation :background ,(doom-color 'base4)) ; Visually highlight if an indentation issue was discovered which emacs already does for us
  `(magit-branch-current  :foreground ,(doom-color 'blue) :box t)
  '(lsp-inlay-hint-face :height 0.85 :italic t :inherit font-lock-comment-face)
  '(lsp-bridge-inlay-hint-face :height 0.85 :italic t :inherit font-lock-comment-face)
)

(custom-set-faces!
  '(adoc-code-face :inherit org-block)
  '(adoc-complex-replacement-face :inherit org-code :weight bold)
  '(adoc-meta-face :inherit org-meta-line)
  '(adoc-typewriter-face :inherit org-code)
  '(adoc-verbatim-face :inherit org-verbatim)
  '(adoc-internal-reference-face :inherit org-link)
  '(adoc-reference-face :inherit org-link)
  `(adoc-emphasis-face :foreground ,(doom-lighten (doom-color 'green) 0.2) :slant italic)
  '(adoc-bold-face :weight bold)
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

;; (custom-set-faces!
;;   `(blamer-face :slant italic :height 90 :weight semi-light :foreground ,(doom-color 'base5)))

(after! highlight-indent-guides
  (setq! highlight-indent-guides-auto-character-face-perc 20))

(setq! tab-width 4)

(setq org-directory "~/.org")

(after! org-id
  (setq org-id-link-to-org-use-id t
        org-id-locations-file (doom-path doom-local-dir "org-id-locations")
        org-id-track-globally t))

(after! org-id (run-with-idle-timer 20 nil 'org-id-update-id-locations))

(after! org-roam (run-with-idle-timer 25 nil 'org-roam-update-org-id-locations))

(after! org
  (setq! org-auto-align-tags nil
         org-tags-column 0
         org-fold-catch-invisible-edits 'show-and-error
         org-ellipsis "…"
         org-indent-indentation-per-level 2)
  
  (auto-fill-mode))

; (custom-declare-face 'org-checkbox-statistics-todo '((t (:inherit (bold font-lock-constant-face org-todo)))) "")

(custom-set-faces!
  ;; '(org-date :foreground "dark goldenrod" :height 0.85)
  '(org-document-title :foreground "#c678dd" :weight bold :height 1.8)
  ;; '(org-drawer :foreground "dark gray" :height 0.8)
  ;; '(org-property-value :height 0.85)
  '(org-ql-view-due-date :foreground "dark goldenrod")
  ;; '(org-special-keyword :foreground "#83898d" :height 0.8)
  ;; '(org-tag :foreground "#83898d" :weight light :height 0.7)
  `(org-code :foreground ,(doom-lighten (doom-color 'warning) 0.3) :extend t)
  '(outline-1 :height 1.5)
  '(outline-2 :height 1.25)
  '(outline-3 :height 1.15)
  `(org-column :height 150 :background ,(doom-color 'base4)
    :slant normal :weight regular :underline nil :overline nil :strike-through nil :box nil :inverse-video nil)
  `(org-column-title :height 150 :background ,(doom-color 'base4) :weight bold :underline t))

(after! org-modern
  (setq! org-modern-priority
         '((?A . "⛔")
           (?B . "𐱄")
           (?C . "▲")
           (?D . "ᐱ")
           (?E . "Ⲷ")
           (?F . "ᐯ")
           (?G . "▼")
           (?H . "𐠠")
           (?I . "҉"))
         org-priority-faces
         '((?A :foreground "red3" :weight bold :height .95)
           (?B :foreground "OrangeRed2" :weight bold)
           (?C :foreground "DarkOrange2" :weight bold)
           (?D :foreground "gold3" :weight bold)
           (?E :foreground "OliveDrab1" :weight bold)
           (?F :foreground "SpringGreen3" :weight bold)
           (?G :foreground "cyan4" :weight bold)
           (?H :foreground "DeepSkyBlue4" :weight bold)
           (?I :foreground "LightSteelBlue3" :weight bold))))

(after! org
  (setq! org-tag-faces `(("LASTMILE" . (:foreground ,(doom-color 'red) :strike-through t))
                         ("HABIT" . (:foreground ,(doom-darken (doom-color 'orange) 0.2)))
                         ("SOMEDAY" . (:slant italic :weight bold))
                         ;; ("finance" . (:foreground "goldenrod"))
                         ;; ("#inbox" . (:background ,(doom-color 'base4) :foregorund ,(doom-color 'base8)))
                         ("#inbox" . (:strike-through t))
                         ("3datax" . (:foreground ,(doom-color 'green)))
                         ("oebb" . (:foreground ,(doom-color 'green)))
                         ("pulswerk" . (:foreground ,(doom-color 'dark-blue)))
                         ("#work" . (:foreground ,(doom-color 'blue)))
                         ;; ("#work" . (:foreground ,(doom-color 'blue)))
                         ("@ikea" . (:foreground ,(doom-color 'yellow)))
                         ("@amazon" . (:foreground ,(doom-color 'yellow)))
                         ;; ("emacs" . (:foreground "#c678dd"))
                         ))
  )

(after! org (run-with-idle-timer 60 t #'org-save-all-org-buffers))

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
           (file ,(doom-path doom-user-dir "templates/template-inbox.org"))
           :empty-lines-after 1)
          ("p" "Project" entry
           (file ,stfl/org-gtd-inbox-absolute)
           (file ,(doom-path doom-user-dir "templates/template-projects.org"))
           :empty-lines-after 1)
          ("s" "scheduled" entry
           (file ,stfl/org-gtd-inbox-absolute)
           (file ,(doom-path doom-user-dir "templates/template-scheduled.org"))
           :empty-lines-after 1)
          ("v" "Versicherung" entry
           (file+headline ,(doom-path org-directory "versicherung.org") "Einreichungen")
           (function stfl/org-capture-template-versicherung)
           :root "~/Documents/Finanzielles/Einreichung Versicherung")
          ("S" "deadline" entry
           (file ,stfl/org-gtd-inbox-absolute)
           (file ,(doom-path doom-user-dir "templates/template-deadline.org"))
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
           (file ,(doom-path doom-user-dir "templates/template-wäsche.org")))
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
:CREATED:  %%U
:date:     [%s]
:betrag:   %%^{Betrag|0}
:oegk:     nil
:generali: nil
:category: %%^{Kategorie|nil|Arzt|Alternativ|Internet|Psycho|Besonders|Apotheke|Vorsorge|Heilbehelfe|Brille|Transport}
:END:

[[file:%s]]

%%?" date title date directory)))
)

(after! org (setq org-archive-location (doom-path org-directory "archive/%s::datetree")))

(after! org (require 'org-checklist))

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
  (setq! org-clock-rounding-minutes 15  ;; Org clock should clock in and out rounded to 5 minutes.
         org-time-stamp-rounding-minutes '(0 15)
         org-duration-format 'h:mm  ;; format hours and don't Xd (days)
         org-clock-report-include-clocking-task t  ;; include current task in the clocktable
         org-log-note-clock-out t
         org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :stepskip0 t :fileskip0 t :hidefiles t :tags t)
         ))

(after! org-clock
  (setq! org-clock-continuously nil)  ;; org-clock-continuously is handled by the advice
  (defvar stfl/org-clock-continous-threshold 60)
  
  (defun stfl/org-time-minutes-ago-rounded (time)
    (/ (org-time-convert-to-integer
        (time-subtract (org-current-time org-clock-rounding-minutes t) time))
       60))

  (defun stfl/org-time-minutes-ago (time)
    (/ (org-time-convert-to-integer
        (time-subtract (org-current-time) time))
       60))

  (defun stfl/org-time-format-ago (time)
    (format "%s (-%dm) (~%dm)"
            (format-time-string (org-time-stamp-format 'with-time t) time)
            (stfl/org-time-minutes-ago time)
            (stfl/org-time-minutes-ago-rounded time)))

  (defadvice! stfl/org-clock-continue? (orig-fn &rest args)
    "Prompt to continue on clock on clock out time if longer than `stfl/org-clock-continous-threshold`."
    :around #'org-clock-in
    (interactive "P")
    (let ((org-clock-continuously
           (or (org-clocking-p)
               (and org-clock-out-time
                    (or (< (stfl/org-time-minutes-ago org-clock-out-time) stfl/org-clock-continous-threshold)
                        (y-or-n-p (format "You stopped another clock at %s; start this one from then? "
                                          (stfl/org-time-format-ago org-clock-out-time))))))))
          (apply orig-fn args)))
  )

(use-package org-clock-csv
  :after org
  :commands +org-clock-project-csv-to-file)

(setq +org-clock-export-dir "~/work/invoice.typ/invoices")
(defun +org-clock-project-csv-to-file (project)
  (interactive
   (list (completing-read "Select project: " stfl/org-gtd-projects)))
  (let* ((org-agenda-files (list (doom-path org-directory project)
                                 (doom-path org-directory "archive" project)))
         (filename (format "%s-org-clock-%s.csv" (format-time-string "%Y-%m") (file-name-base project)))
         (filepath (doom-path +org-clock-export-dir filename)))
    (org-clock-csv-to-file filepath)))

(map! :map org-mode-map
      :localleader
      :prefix "c"
      :desc "Export project clock entries" "C" #'+org-clock-project-csv-to-file)

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
  (setq! org-todo-keywords
         '((sequence
            "TODO(t)"  ; A task that needs doing & is ready to do
            "NEXT(n)"  ; Task is next to be worked on.
            "WAIT(w)"  ; Something external is holding up this task
            "PROJ(p)"  ; Project with multiple task items.
            "EPIC(e)"  ; A set of Projects
            "|"
            "DONE(d@)"  ; Task successfully completed
            "IDEA(i)"   ; An unconfirmed and unapproved task or notion
            "KILL(k@)")) ; Task was cancelled, aborted or is no longer applicable
         org-todo-repeat-to-state "NEXT"))

(custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-idea    '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-epic    '((t (:inherit (bold org-cite org-todo)))) "")
(custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-next    '((t (:inherit (bold font-lock-keyword-face org-todo)))) "")

(custom-set-faces!
  `(+org-todo-cancel :foreground ,(doom-blend (doom-color 'red) (doom-color 'base5) 0.35) :inherit (bold org-done))
  `(+org-todo-idea   :foreground ,(doom-darken (doom-color 'green) 0.4) :inherit (bold org-todo)))

(after! org
  (setq! org-todo-keyword-faces
         '(("[-]"  . +org-todo-active)
           ("NEXT" . +org-todo-next)
           ("WAIT" . +org-todo-onhold)
           ("IDEA" . +org-todo-idea)
           ("PROJ" . +org-todo-project)
           ("EPIC" . +org-todo-epic))))

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

(after! org
  (setq org-tag-alist '((:startgrouptag)
                        ("Context" . nil)
                        (:grouptags)
                        ;; ("@home" . ?h)
                        ;; ("@office". ?o)
                        ("@sarah" . ?s)
                        ("@lena" . ?l)
                        ;; ("@kg" . ?k)
                        ("@jg" . ?j)
                        ("@mfg" . ?m)
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
                        ("emacs" . ?-)
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
      "s" #'org-toggle-sticky-agenda

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
       org-stuck-projects '("-SOMEDAY/+PROJ" ("NEXT" "WAIT") ("WAITING") "")
       org-agenda-sticky t  ;; q key will NOT bury agenda buffers
       )

(setq stfl/org-agenda-primary-work-tags '("3datax" "@3datax" "#3datax"
                                          "oebb" "@oebb" "#oebb"))

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
      stfl/org-gtd-projects '("emacs.org" "freelance.org" "geschenke.org" "media.org" "projects.org"
                              "3datax.org" "pulswerk.org" "versicherung.org" "ikea.org" "oebb.org")
      stfl/org-roam-absolute (doom-path org-directory "roam/"))

(after! org
  (setq org-agenda-diary-file (doom-path org-directory "diary.org")
        org-agenda-files `(,stfl/org-gtd-inbox
                           ,stfl/org-gtd-inbox-orgzly
                           ,stfl/org-gtd-todo
                           ,@stfl/org-gtd-projects
                           ;; ,@(file-expand-wildcards (doom-path stfl/proxmox-support-dir "**/*.org"))i
                           )))

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
                              ;; ,(prio-deadline>= org-priority-default)
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
        ("wB" "Backlog #work w/ Primary Work"
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

(after! org-contrib
  (require 'org-checklist))

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

;; (setq! auth-sources 'password-store)

;; (use-package! define-word
;;   :after org
;;   :config
;;   (map! :after org
;;         :map org-mode-map
;;         :leader
;;         :desc "Define word at point" "@" #'define-word-at-point))

(setq org-pandoc-options '((standalone . t) (self-contained . t)))

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

(after! vterm
  (setq! vterm-max-scrollback 200000
         ;; vterm-min-window-width 5000
         )) ;; do not wrap long lines per default

(map!
 :after vterm
 :map vterm-mode-map
 "C-c C-x" #'vterm--self-insert
 :n "C-r" #'vterm--self-insert
 :n "C-j" #'vterm--self-insert
 :i "C-j" #'vterm--self-insert
 :i "TAB" #'vterm-send-tab
 :i "<tab>" #'vterm-send-tab)

(after! vterm
  (defun vterm-send-return ()
    "Send `C-m' to the libvterm."
    (interactive)
    (deactivate-mark)
    (when vterm--term
      (process-send-string vterm--process "\C-m"))))

(after! vterm
  (setq! vterm-tramp-shells '(("docker" "/bin/sh")
                              ("ssh" "/bin/bash"))))

(use-package! typst-ts-mode
  :defer t
  ;; :init
  ;; (setq! typst-ts-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  :config
  (setq!
   typst-ts-watch-options "--open"
   typst-ts-indent-offset 2
   typst-ts-enable-raw-blocks-highlight t)
  (map! :map typst-ts-mode-map
        "C-c C-c" #'typst-ts-tmenu
        :localleader
        :desc "Compile" "c" #'typst-ts-compile
        :desc "Watch" "w" #'typst-ts-watch-mode
        :desc "Menu" "m" #'typst-ts-tmenu)
  ;; (add-hook! 'typst-ts-mode-hook #'lsp-deferred)
  (add-hook! 'typst-ts-mode-hook #'eglot-ensure))

(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))

(after! eglot
  (add-to-list 'eglot-server-programs
               `(typst-ts-mode . ("tinymist"))))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst") t)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                    :activation-fn (lsp-activate-on "typst")
                    :server-id 'tinymist)))

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-space-mode))

(after! flycheck
  (map! :map flycheck-mode-map
        :leader
        "c x" #'consult-flycheck))

(map! :leader ":" #'ielm)

(after! lsp-treemacs
  (lsp-treemacs-sync-mode 1))

(map! :after lsp-mode
      :map lsp-mode-map
      :leader
      :prefix ("c" . "+code")
      :desc "Diagnostic for Workspace" "X" #'lsp-treemacs-errors-list)

(after! lsp-mode
  (setq! lsp-inlay-hint-enable t
         lsp-headerline-breadcrumb-enable t
         lsp-ui-sideline-enable nil)
  )

(when (executable-find "emacs-lsp-booster")
  (after! lsp-mode
    (setq! lsp-use-plists t)
    
    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection)))  ;; native json-rpc
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (append '("emacs-lsp-booster" "--disable-bytecode" "--") orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

(use-package! lsp-bridge
  :disabled t
  :config
  (setq! lsp-bridge-user-langserver-dir (doom-path doom-user-dir "langserver")
         lsp-bridge-enable-inlay-hint t
         lsp-bridge-enable-hover-diagnostic t
         lsp-bridge-enable-signature-help nil 
         lsp-bridge-enable-auto-format-code nil
         lsp-bridge-enable-org-babel t
         lsp-bridge-log-level 'default
         acm-enable-capf t
         acm-enable-org-roam t
         )
  
  (set-lookup-handlers! 'lsp-bridge-mode
    :definition #'lsp-bridge-peek
    ;; :definition #'lsp-bridge-find-def
    :references #'lsp-bridge-find-references
    :documentation #'lsp-bridge-popup-documentation
    :implementations #'lsp-bridge-find-impl
    :type-definition #'lsp-bridge-find-type-def)

  (map! :map lsp-bridge-peek-keymap
        :g "C-j" #'lsp-bridge-peek-list-next-line
        :g "C-k" #'lsp-bridge-peek-list-prev-line
        :g "C-S-j" #'lsp-bridge-peek-file-content-next-line
        :g "C-S-k" #'lsp-bridge-peek-file-content-prev-line
        :g "RET" #'lsp-bridge-peek-jump
        :g "C-SPC" #'lsp-bridge-peek-jump
        :g "ESC" #'lsp-bridge-peek-abort)
  
  (map! :map lsp-bridge-mode-map
        :leader
        :n "c r" #'lsp-bridge-rename
        :n "c a" #'lsp-bridge-code-action
        :n "c f" #'lsp-bridge-code-format
        )
  
  (map! :map acm-mode-map
        :i "C-j" #'acm-select-next
        :i "C-k" #'acm-select-prev)
  
  (acm-mode t)
  (global-lsp-bridge-mode)
  ;; (lsp-bridge-semantic-tokens-mode t)
  )

(use-package! flyover
  :after flycheck
  :config
  (setq! flyover-checkers '(flycheck)
         ;; flyover-levels '(error warning info)  ; Show all levels
         ;; flyover-levels '(error warning)
         flyover-levels '(error)
         
         flyover-use-theme-colors t ;; Use theme colors for error/warning/info faces
         flyover-background-lightness 35; Adjust background lightness (lower values = darker)
         ;; flyover-percent-darker 40 ;; Make icon background darker than foreground
         flyover-text-tint nil ; 'lighter ;; or 'darker or nil
         ;; flyover-text-tint-percent 50 ;; "Percentage to lighten or darken the text when tinting is enabled."
         flyover-debug nil ;; Enable debug messages
         ;; flyover-debounce-interval 0.2 ;; Time in seconds to wait before checking and displaying errors after a change
         
         ;; flyover-wrap-messages t ;; Enable wrapping of long error messages across multiple lines
         flyover-max-line-length 100 ;; Maximum length of each line when wrapping messages
         
         flyover-hide-checker-name t
         
         flyover-show-virtual-line t ;;; Show an arrow (or icon of your choice) before the error to highlight the error a bit more.
         ;; flyover-virtual-line-type 'straight-arrow
         
         flyover-line-position-offset 1
         
         flyover-show-at-eol t ;;; show at end of the line instead.
         flyover-hide-when-cursor-is-on-same-line t ;;; Hide overlay when cursor is at same line, good for show-at-eol.
         flyover-virtual-line-icon " ──► " ;;; default its nil
         )
  (add-hook 'flycheck-mode-hook #'flyover-mode)
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

(after! lsp-bridge
  (setq! lsp-bridge-python-multi-lsp-server "basedpyright_ruff"))

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

(use-package! rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(map! :mode rustic-mode
      :map rustic-mode-map
      :localleader
      :desc "rerun test" "t r" #'rustic-cargo-test-rerun)

(after! rustic
  (when (executable-find "cargo-nextest")
    (setq! rustic-cargo-test-runner 'nextest)))

(after! lsp-rust
  (setq! lsp-rust-analyzer-binding-mode-hints t
  ;;        lsp-rust-analyzer-display-chaining-hints t
  ;;        lsp-rust-analyzer-display-closure-return-type-hints t
         lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
  ;;        lsp-rust-analyzer-display-parameter-hints t
  ;;        lsp-rust-analyzer-hide-named-constructor t
         lsp-rust-analyzer-max-inlay-hint-length 40  ;; otherwise some types can get way out of hand
         )
  )

(after! eglot
  (setq eglot-workspace-configuration
        (plist-put eglot-workspace-configuration
                   :rust-analyzer
                   '(:inlayHints (:maxLength 40)))))

(after! (rust-mode dap-mode)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

(after! nix-mode
  (set-formatter! 'alejandra '("alejandra" "--quiet")
    :modes '(nix-mode)))

(after! lsp-bridge
  (setq! lsp-bridge-nix-lsp-server "nil"))

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
  :after sql-mode)

(use-package! edbi
  :commands 'edbi:open-db-viewer)

(use-package! edbi-minor-mode
  :after sql-mode
  :hook sql-mode-hook)
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
(setq lsp-ltex-active-modes '(text-mode
                              bibtex-mode
                              context-mode
                              latex-mode
                              markdown-mode
                              org-mode
                              rst-mode
                              adoc-mode))

(use-package! lsp-ltex
  :after lsp-ltex-active-modes
  :hook (adoc-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-server-store-path "~/.nix-profile/bin/ltex-ls"
        lsp-ltex-version "16.0.0"
        lsp-ltex-mother-tongue "de-AT"
        lsp-ltex-user-rules-path (doom-path doom-user-dir "lsp-ltex")))

(use-package! ssh-config-mode :defer t)

(use-package! bitbake-ts-mode
  :config (add-to-list 'auto-mode-alist '("\\.inc$" . bitbake-ts-mode)))

(after! lsp-bridge
  (add-to-list 'lsp-bridge-single-lang-server-mode-list
               ;; '(bitbake-ts-mode . "bitbake-language-server")
               '(bitbake-ts-mode . "language-server-bitbake"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'bitbake-ts-mode-hook t))

(use-package! meson-mode
  :config (add-hook! 'meson-mode-hook #'company-mode))

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(after! lsp-bridge
  (setq! lsp-bridge-c-lsp-server "ccls"))

(defun run-ctest (arg)
  (interactive "P")
  (let ((projectile-project-test-cmd "cmake --build build && ctest --test-dir build --output-on-failure --rerun-failed"))
    (projectile-test-project arg)))


(map! :mode c++-mode
      :map c++-mode-map
      :localleader 
      :prefix ("t" "test")
      :n "t" #'run-ctest
      ;; :n "t" #'gtest-run-at-point
      ;; :n "T" #'gtest-run
      ;; :n "l" #'gtest-list
      )

(use-package! turbo-log
  :after prog-mode
  :config
  (map! :leader
        "l l" #'turbo-log-print
        "l i" #'turbo-log-print-immediately
        "l h" #'turbo-log-comment-all-logs
        "l s" #'turbo-log-uncomment-all-logs
        "l [" #'turbo-log-paste-as-logger
        "l ]" #'turbo-log-paste-as-logger-immediately
        "l d" #'turbo-log-delete-all-logs)
  (setq! turbo-log-msg-format-template "\"🚀: %s\""
         turbo-log-allow-insert-without-treesit-p t))

(use-package! just-mode
  :defer t)

(use-package! ztree)

;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

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

(use-package! magit-todos
  :after magit
  :config
  (setq! magit-todos-exclude-globs '(".git/" "node_modules/"))
  (magit-todos-mode 1))

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
        ediff-split-window-function 'split-window-horizontally
        ediff-floating-control-frame t
        ))

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
         blamer-max-commit-message-length 80
         ;; blamer-max-lines 100
         blamer-type 'visual
         ;; blamer-type 'posframe-popup
         ;; blamer-type 'overlay-popup
         blamer-min-offset 40)
  
  ;; (custom-set-faces!
  ;;   `(blamer-face :inherit font-lock-comment-face
  ;;     :slant italic
  ;;     :font "JetBrains Mono"
  ;;     ;; :height 0.9
  ;;     :background unspecified
  ;;     ;; :weight semi-light
  ;;     ;; :foreground ,(doom-color 'base5)
  ;;     ))
  
  (add-hook! org-mode-hook (λ! (blamer-mode 0))))

(map!
      ;; "C-c a" #'aidermacs-transient-menu
      :leader
      (:prefix ("j" . "AI")
       ;; "m" #'gptel-menu
       ;; "j" #'gptel
       ;; "C-g" #'gptel-abort
       ;; "C-c" #'gptel-abort
       ;; :desc "Toggle context" "C" #'gptel-add
       ;; "s" #'gptel-system-prompt
       ;; "w" #'gptel-rewrite-menu
       ;; "t" #'gptel-org-set-topic
       ;; "P" #'gptel-org-set-properties
       
       "a" #'aidermacs-transient-menu
       ;; "a" #'aider-transient-menu
       
       ;; "e" #'elysium-query
       
       (:prefix ("c" . "Copilot Chat")
        ;; "" #'copilot-chat-reset  ;; reset everything including history, buffers and frontend.
        "c" #'copilot-chat-display  ;; display copilot chat buffers.
        "s" #'copilot-chat-explain-symbol-at-line  ;; ask Copilot to explain symbol under point.
        "e" #'copilot-chat-explain  ;; ask copilot to explain selected code.
        "r" #'copilot-chat-review  ;; ask copilot to review selected code.
        "d" #'copilot-chat-doc  ;; ask copilot to document selected code.
        "f" #'copilot-chat-fix  ;; ask copilot to fix selected code.
        "o" #'copilot-chat-optimize  ;; ask copilot to optimize selected code.
        "t" #'copilot-chat-test  ;; ask copilot to write tests for selected code.
        ;; :n "" #'copilot-chat-custom-prompt-selection  ;; ask for a prompt in minibuffer and pastes selection after it before sending it to copilot.
        "b" #'copilot-chat-add-current-buffer  ;; add current buffer to copilot chat. Its content will be sent with every request.
        "B" #'copilot-chat-del-current-buffer  ;; remove current buffer.
        "l" #'copilot-chat-list  ;; open buffer list.
        ;; "" #'copilot-chat-prompt-history-previous  ;; insert previous prompt from history in prompt buffer.
        ;; "" #'copilot-chat-prompt-history-next  ;; insert next prompt from history in prompt buffer.
        "a" #'copilot-chat-ask-and-insert  ;; ask for a custom prompt and write answer in current buffer at point.
        "m" #'copilot-chat-insert-commit-message  ;; Insert in the current buffer a copilot generated commit message.
        )))



(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :after prog-mode
  :config
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun +copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.
  
  ;; Bind the custom function to <tab> in Evil's insert state
  ;; (evil-define-key 'insert 'global (kbd "<tab>") #'+copilot-tab-or-default)

  (map! :map copilot-completion-map
        "<tab>" #'+copilot-tab-or-default
        "TAB" #'+copilot-tab-or-default
        ;; :i "C-TAB" #'copilot-accept-completion-by-word
        ;; :i "C-<tab>" #'copilot-accept-completion-by-word
        "C-S-n" #'copilot-next-completion
        ;; :i "C-<tab>" #'copilot-next-completion
        "C-S-p" #'copilot-previouse-completion
        ;; :i "C-<iso-lefttab>" #'copilot-previouse-completion
        )
  
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  
  (setq! copilot-indent-offset-warning-disable t
         copilot-max-char-warning-disable t)
  
  (setq copilot-lsp-settings '(:github (:copilot (:selectedCompletionModel "gpt-41-copilot"))))
  )

(use-package copilot-chat
  :after org
  :commands (copilot-chat-insert-commit-message copilot-chat-fix copilot-chat-doc)
  :config (setq! copilot-chat-model "claude-3.7-sonnet"
                 copilot-chat-frontend 'org)
  
  ;; (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)
  ;; Or call manually (copilot-chat-insert-commit-message) when in the commit message buffer.
  )

(use-package! codeium
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
  (setq codeium/metadata/api_key (password-store-get "API/Codeium"))

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

;; (use-package! gptel
;;   :after password-store
;;   :commands (gptel gptel-menu)
;;   :config
(after! gptel
  (setq! gptel-default-mode 'org-mode
         ;; gptel-response-prefix-alist '((org-mode . "**** Answer"))
         gptel-api-key (password-store-get "API/OpenAI-emacs")
         ;; gptel-model 'gpt-4o
         gptel-model 'gemini-pro
         ;; 'gpt-4.5-preview
         gptel-log-level 'info
         ;; gptel-use-curl nil
         gptel-use-curl t
         gptel-stream t)

  (defun +gptel-font-lock-update (pos pos-end)
    ;; used with the gptel-post-response-functions hook but swollows the arguments
    (font-lock-update))
  
  ;; reload font-lock to fix syntax highlighting of org-babel src blocks
  (add-hook 'gptel-post-response-functions '+gptel-font-lock-update)

  (gptel-make-gemini "Gemini" :stream t
    :key (password-store-get "API/Gemini-emacs"))
  
  (gptel-make-anthropic "Claude"          ;Any name you want
    :stream t                             ;Streaming responses
    :key (password-store-get "API/Claude-emacs"))
  
  (gptel-make-perplexity "Perplexity"          ;Any name you want
    :stream t                             ;Streaming responses
    :key (password-store-get "API/Perplexity-emacs-pro-ste.lendl"))
    
  ;; Perplexity offers an OpenAI compatible API
  ;; NOTE https://docs.perplexity.ai/guides/model-cards
  ;; (gptel-make-openai "Perplexity"         ;Any name you want
  ;;   :host "api.perplexity.ai"
  ;;   :key (password-store-get "API/Perplexity-gptel")
  ;;   :endpoint "/chat/completions"
  ;;   :stream t
  ;;   :models '(sonar
  ;;             sonar-pro
  ;;             sonar-reasoning
  ;;             sonar-reasoning-pro
  ;;             sonar-deep-research	
  ;;             r1-1776	
  ;;             ))

  ;; (set-popup-rules!
  ;;   '(("^\\*ChatGPT\\*" :select t :quit nil :ttl nil :modeline t :persist t)
  ;;     ("^\\*Perplexity\\*" :select t :quit nil :ttl nil :modeline t :persist t)
  ;;     ("^\\*Claude\\*"  :select t :quit nil :ttl nil :modeline t :persist t)))
  
  (setf (alist-get 'perplexity gptel-directives) "You are Perplxity, a helpful search assistant, living in Emacs.

Your task is to deliver a concise and accurate response to a user's query, drawing from the given search results. Your answer must be precise, of high-quality, and written by an expert using an unbiased and journalistic tone. It is EXTREMELY IMPORTANT to directly answer the query. NEVER say 'based on the search results' or start your answer with a heading or title. Get straight to the point. Your answer must be written in the same language as the query, even if language preference is different.

You MUST cite the most relevant search results that answer the query. Do not mention any irrelevant results. You MUST ADHERE to the following instructions for citing search results:

- To cite a search result, enclose its index located above the summary with brackets at the end of the corresponding sentence, for example 'Ice is less dense than water[1][2].'
or 'Paris is the capital of France[1][4][5].'
- NO SPACE between the last word and the citation, and ALWAYS use brackets. Only use this format to cite search results. 
- If you don't know the answer or the premise is incorrect, explain why. If the search results are empty or unhelpful, answer the query as well as you can with existing knowledge.
- ALWAYS include a References section at the end of your answer. in the format
  #### References
  [1] <Title> <link>
  [2] <Title> <link>

Use markdown to format paragraphs, lists, tables, and quotes whenever possible.

- Use headings level 4 to separate sections of your response, like '#### Header'.
- Use single new lines for lists and double new lines for paragraphs.
- Use markdown to render images given in the search results.")
  
  (setf (alist-get 'cpp gptel-directives) "You are an expert C++ developer using C++20. ONLY use C++20 features availible in gcc12.
Do not use concepts. For functions, methods and variables use the style 'auto method() -> RetType'
Reply concisely. Wrap source code in a ```cpp block.")
  
  ;; (transient-suffix-put 'gptel-menu (kbd "RET") :key "<f8>")
  )

;; (after! gptel
;;   (defadvice! +gptel--rename-buffer (&rest args)
;;     "rename gptel buffers to a *gptel prefix"
;;     :filter-return #'gptel
;;         (unless (string-prefix-p "*gptel" (buffer-name))
;;           (rename-buffer (generate-new-buffer-name (concat "*gptel " (buffer-name))) t)
;;           (message "renamed gptel buffer to %s" (buffer-name))))
;;   )

(use-package! aidermacs
  :commands (aidermacs-transient-menu)
  :init
  (add-hook 'aidermacs-before-run-backend-hook
          (lambda ()
            (message "Setting up API keys")
            (setenv "OPENAI_API_KEY" (password-store-get "API/OpenAI-emacs"))
            (setenv "ANTHROPIC_API_KEY" (password-store-get "API/Claude-emacs"))
            (setenv "GEMINI_API_KEY" (password-store-get "API/Gemini-emacs"))
            (setenv "PERPLEXITYAI_API_KEY" (password-store-get "API/Perplexity-emacs-pro-ste.lendl"))
            (setenv "OPENROUTER_API_KEY" (password-store-get "API/Openrouter-emacs"))
            ))
  :config
  (setq! aidermacs-default-chat-mode 'architect
         ;; aidermacs-default-model "openrouter/google/gemini-2.5-pro"
         aidermacs-default-model "openrouter/anthropic/claude-sonnet-4"
         aidermacs-architect-model "openrouter/anthropic/claude-sonnet-4"
         ;; aidermacs-architect-model "openrouter/x-ai/grok-4"
         ;; aidermacs-weak-model "openrouter/google/gemini-2.5-flash"
         aidermacs-weak-model "openrouter/deepseek/deepseek-r1-0528"
         ;; aidermacs-backend 'vterm
         aidermacs-backend 'comint
         aidermacs-watch-files t
         aidermacs-extra-args '("--thinking-tokens" "8k" "--reasoning-effort" "medium"))
  (set-popup-rule! "^\\*aidermacs:"
    :select t
    :size 0.3
    :quit nil
    :ttl nil)
  )
