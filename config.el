;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "Stefan Lendl"
      user-mail-address "contact@stfl.dev")

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
(setq auto-save-visited-interval 30)
(auto-save-visited-mode 1)

(setq calendar-week-start-day 1)

(when (executable-find "brave")
  (setopt browse-url-browser-function 'browse-url-chromium
          browse-url-chromium-program "brave"))

(global-set-key [M-drag-mouse-2] #'mouse-drag-vertical-line)

;; (defun mouse-drag-left-line (start-event)
;;   "Change the width of a window by dragging on a vertical line.
;; START-EVENT is the starting mouse event of the drag action."
;;   (interactive "e")
;;   (mouse-drag-line start-event 'left))

;; (global-set-key [left-fringe drag-mouse-1] #'mouse-drag-left-line)

(with-eval-after-load 'evil-snipe
  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'visible))

(map! :leader "f ." #'find-file-at-point)

(setopt tab-always-indent 'complete)

(with-eval-after-load 'evil-escape
  (setq evil-escape-key-sequence "jk"))

(use-package drag-stuff
  :defer t
  :init
  (map! "<M-up>"    #'drag-stuff-up
        "<M-down>"  #'drag-stuff-down
        "<M-left>"  #'drag-stuff-left
        "<M-right>" #'drag-stuff-right))

(setq gc-cons-threshold most-positive-fixnum)

(run-with-idle-timer 1.2 t 'garbage-collect)

(setopt focus-follows-mouse 'auto-raise
        mouse-autoselect-window nil)

(with-eval-after-load 'consult
  (setopt consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                            "--color=never"
                            ;; "--full-path"
                            ;; "--absolute-path"
                            "--hidden"
                            "--exclude .git"
                            (if (featurep :system 'windows) "--path-separator=/")))

  (defun stfl/consult-fd (&optional arg)
    (interactive "P")
    (let ((consult-fd-args (append consult-fd-args (and arg '("--no-ignore")))))
      (consult-fd)))

  (defun stfl/projectile-find-file (&optional arg)
    (interactive "P")
    (if arg (stfl/consult-fd arg)
      (projectile-find-file)))

  ;; (defun stfl/projectile-find-file (&optional arg)
  ;;   (interactive "P")
  ;;   (let ((projectile-git-fd-args (concat projectile-git-fd-args (and arg " --no-ignore-vcs"))))
  ;;     (projectile-find-file)))

  (defun stfl/consult-fd-dir (&optional arg)
    (interactive "P")
    (let ((consult-fd-args (append consult-fd-args '("--type d") (and arg '("--no-ignore")))))
      (consult-fd)))

  (map! :leader
        "SPC" #'stfl/projectile-find-file
        :prefix "f"
        "l" #'stfl/consult-fd
        "L" #'stfl/consult-fd-dir))

(setq doom-theme 'doom-one)

(setopt display-line-numbers-type t)
(setopt which-key-idle-delay 0.3)

(let ((font "JetBrains Mono Nerd Font Mono"))
  (setq doom-font (font-spec :family font :size 13)
        doom-variable-pitch-font (font-spec :family font)
        doom-big-font (font-spec :family font :size 20)))

(custom-set-faces!
  `(whitespace-indentation :background ,(doom-color 'base4)) ; Visually highlight if an indentation issue was discovered which emacs already does for us
  `(magit-branch-current  :foreground ,(doom-color 'blue) :box t)
  '(lsp-inlay-hint-face :height 0.85 :italic t :inherit font-lock-comment-face)
  '(lsp-bridge-inlay-hint-face :height 0.85 :italic t :inherit font-lock-comment-face)

  ;; Subtle dark-red wavy underline for misspellings (blend red into bg so it
  ;; reads as a muted alarm, not a bright distraction). Mirrors the doom-themes
  ;; idiom of using `doom-blend' against `bg' for grayed-out variants.
  `(spell-fu-incorrect-face
    :foreground unspecified :background unspecified :inherit unspecified
    :underline (:style wave :color ,(doom-blend 'red 'bg 0.5)))
)

(setopt tab-width 4)

(setq org-directory "~/.org")

(with-eval-after-load 'org-id
  (setq org-id-link-to-org-use-id t
        org-id-locations-file (doom-path doom-local-dir "org-id-locations")
        org-id-track-globally t))

(with-eval-after-load 'org-roam
  (run-with-idle-timer
   25 nil
   (lambda ()
     ;; `org-id-files' carries files over from the previous session, and the
     ;; checksum lets `org-id-update-id-locations' skip an unchanged scan.
     (setq org-id-files nil
           org-id--locations-checksum nil)
     (org-roam-update-org-id-locations))))

(use-package agile-gtd
  :after org
  :config
  ;; Additional agenda files not managed by agile-gtd projects
  (setq org-agenda-files (mapcar (lambda (f) (expand-file-name f org-directory))
                                 '("inbox-orgzly.org"
                                   "geschenke.org"
                                   "media.org"
                                   "projects.org"
                                   "versicherung.org"
                                   "ikea.org"
                                   "emacs.org"
                                   "homelab.org"
                                   "cafe-glas.org"
                                   ;; A directory: Org lists its .org files on every agenda build
                                   "decisions/")))
  (setq agile-gtd-projects '((:tag "oebb"       :name "ÖBB"        :key ?o)
                             (:tag "origina"    :name "Origina"    :key ?i)
                             (:tag "momentedge" :name "MomentEdge" :key ?m)
                             (:tag "pulswerk"   :name "Pulswerk"   :key ?p)
                             (:tag "freelance"  :name "Freelance")
                             ;; (:tag "homelab"    :name "Homelab")
                             ;; (:tag "emacs"      :name "Emacs")
                             (:tag "glas"       :name "Café Glas"  :file "cafe-glas.org")))
  (agile-gtd-enable))

(use-package mcp-server-lib
  :custom
  (mcp-server-lib-install-directory (expand-file-name "bin/" doom-emacs-dir))
  :config
  (require 'mcp-server-lib-commands)
  (unless (file-exists-p (mcp-server-lib-installed-script-path))
    (mcp-server-lib-install)))

(use-package org-records-mcp
  :after (org agile-gtd)
  :config
  (let ((source (org-records-mcp--package-script-path))
        (target (org-records-mcp--installed-script-path)))
    (unless (and (file-exists-p target)
                 (string= (with-temp-buffer (insert-file-contents source)
                                            (buffer-string))
                          (with-temp-buffer (insert-file-contents target)
                                            (buffer-string))))
      (make-directory (file-name-directory target) t)
      (copy-file source target t)
      (set-file-modes target #o755)))
  (if mcp-server-lib--running
      (message "org-records-mcp: MCP server already running, skipping start")
    (mcp-server-lib-start))
  ;; A standing registration: the tools survive an Emacs restart under a
  ;; running stdio bridge, whose enable/disable pairs count above it.
  (org-records-mcp-enable))

(with-eval-after-load 'org
  (setopt org-auto-align-tags nil
          org-tags-column 0
          org-fold-catch-invisible-edits 'show-and-error
          org-ellipsis "…"
          org-indent-indentation-per-level 2)

  (auto-fill-mode))

; (custom-declare-face 'org-checkbox-statistics-todo '((t (:inherit (bold font-lock-constant-face org-todo)))) "")

(custom-set-faces!
  '(org-document-title :foreground "#c678dd" :weight bold :height 1.8)
  '(org-ql-view-due-date :foreground "dark goldenrod")
  `(org-agenda-clocking :background nil :box ,(doom-color 'fg))
  `(org-code :foreground ,(doom-lighten (doom-color 'warning) 0.3) :extend t)
  '(outline-1 :height 1.5)
  '(outline-2 :height 1.25)
  '(outline-3 :height 1.15)
  `(org-column :height 130 :background ,(doom-color 'base4)
    :slant normal :weight regular :underline nil :overline nil :strike-through nil :box nil :inverse-video nil)
  `(org-column-title :height 150 :background ,(doom-color 'base4) :weight bold :underline t))

(with-eval-after-load 'org
  (setq org-agenda-clock-consistency-checks
        `(:max-duration "8:00"
          :min-duration 0
          :max-gap "0:05"
          :gap-ok-around ("4:00")
          :default-face nil ;; ((:background "DarkRed") (:foreground "white"))
          :overlap-face ((:foreground ,(doom-color 'error)))
          :gap-face ((:foreground ,(doom-color 'dark-cyan)))
          :no-end-time-face ((:foreground ,(doom-color 'warning)))
          :long-face nil
          :short-face nil))
  )

(with-eval-after-load 'org
  (setopt org-tag-faces `((,agile-gtd-lastmile-tag . (:foreground ,(doom-color 'red) :strike-through t))
                          (,agile-gtd-habit-tag . (:foreground ,(doom-darken (doom-color 'orange) 0.2)))
                          (,agile-gtd-someday-tag . (:slant italic :weight bold))
                          ;; ("finance" . (:foreground "goldenrod"))
                          ;; ("#inbox" . (:background ,(doom-color 'base4) :foregorund ,(doom-color 'base8)))
                          ("#inbox" . (:strike-through t))
                          ("3datax" . (:foreground ,(doom-color 'green)))
                          ("oebb" . (:foreground ,(doom-color 'green)))
                          ("pulswerk" . (:foreground ,(doom-color 'dark-blue)))
                          (,agile-gtd-work-tag . (:foreground ,(doom-color 'blue)))
                          ;; ("#work" . (:foreground ,(doom-color 'blue)))
                          ("@ikea" . (:foreground ,(doom-color 'yellow)))
                          ("@amazon" . (:foreground ,(doom-color 'yellow)))
                          ;; ("emacs" . (:foreground "#c678dd"))
                          ))
  )

(with-eval-after-load 'org
  (setq org-startup-indented 'indent
        org-startup-folded 'fold
        org-startup-with-inline-images t
        ;; org-image-actual-width (round (* (font-get doom-font :size) 25))
        org-image-actual-width (list (* (default-font-width) 40))
        org-image-max-width 'window
        ))
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'turn-off-auto-fill)

;; (bind-key "<f6>" #'link-hint-copy-link)
(map! :after org
      :map org-mode-map
      :leader
      :prefix "n"
      :desc "Revert all org buffers" "R" #'org-revert-all-org-buffers
      )

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Revert all org buffers" "R" #'org-revert-all-org-buffers
      "N" #'org-add-note

      :prefix "l"
      "o" #'org-open-at-point
      "g" #'eos/org-add-ids-to-headlines-in-file

      :prefix "d"
      "c" #'org-cancel-repeater
      )

(defun stfl/build-my-roam-files () (file-expand-wildcards (doom-path org-directory "roam/**/*.org")))

(with-eval-after-load 'org-refile
  (defun stfl/refile-to-roam ()
    (interactive)
    (let ((org-refile-targets '((stfl/build-my-roam-files :maxlevel . 1))))
      (call-interactively 'org-refile))))

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

(with-eval-after-load 'org
  (setq org-capture-templates
        (append
         (cl-remove-if (lambda (template)
                         (equal "v" (car-safe template)))
                       org-capture-templates)
         `(("v" "Versicherung" entry
            (file+headline ,(doom-path org-directory "versicherung.org") "Einreichungen")
            (function stfl/org-capture-template-versicherung)
            :root "~/Documents/Finanzielles/Einreichung Versicherung")))))

(setq stfl/org-roam-absolute (doom-path org-directory "roam/"))
(with-eval-after-load 'org-roam
  (setopt org-roam-capture-templates
          `(("d" "default" plain "%?"
             :target (file+head ,(doom-path stfl/org-roam-absolute "%<%Y%m%d%H%M%S>-${slug}.org")
                                "#+title: ${title}\n")
             :unnarrowed t))))

(with-eval-after-load 'org
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
      (format "* SVS [%s] %s
:PROPERTIES:
:CREATED:  %%U
:date:     [%s]
:betrag:   %%^{Betrag|0}
:svs:      nil
:generali: nil
:category: %%^{Kategorie|nil|Arzt|Alternativ|Internet|Psycho|Besonders|Apotheke|Vorsorge|Heilbehelfe|Brille|Transport}
:END:

[[file:%s]]

%%?" date title date directory)))
)

(with-eval-after-load 'org (require 'org-checklist))

(with-eval-after-load 'org-clock
  (setopt org-clock-rounding-minutes 15  ;; Clock in and out rounded to quarter hours.
          org-time-stamp-rounding-minutes '(0 15)
          org-duration-format 'h:mm  ;; format hours and don't Xd (days)
          org-clock-report-include-clocking-task t  ;; include current task in the clocktable
          org-log-note-clock-out t
          org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :stepskip0 t :fileskip0 t :hidefiles t :tags t)
          ))

(with-eval-after-load 'org-clock
  ;; Continuation is decided per project by `org-clock-projects'.
  (setopt org-clock-continuously nil))

(with-eval-after-load 'org
  (defun stfl/org-read-date-time ()
    (let ((now (org-current-time org-clock-rounding-minutes t)))
      (org-read-date t t nil nil now (format-time-string "%H:%M" now))))

  (defun stfl/org-clock-in-at ()
    (interactive)
    (require 'org-clock)
    (let ((time (stfl/org-read-date-time))
          (org-clock-continuously (org-clocking-p)))

      (when (org-clocking-p)
        ;; Sanity check to avoid negative clock times -> best resolve manually
        (when (> 0 (time-subtract time org-clock-start-time))
          (error (format "Manually clocking in while another LATER clock is running! \"%s\" started at %s"
                         org-clock-heading (format-time-string (org-time-stamp-format 'with-time t) org-clock-start-time))))
        (org-clock-out nil nil time))

      (org-clock-in nil time)))

  (defun stfl/org-clock-out-at ()
    (interactive)
    (when (org-clocking-p) (org-clock-out nil nil (stfl/org-read-date-time))))

  (map! :map org-mode-map
        :localleader
        :prefix "c"
        :desc "clock IN at time" "I" #'stfl/org-clock-in-at
        :desc "clock OUT at time" "O" #'stfl/org-clock-out-at))

(with-eval-after-load 'org-clock
  (setopt org-clock-auto-clock-resolution nil))

(use-package org-clock-projects
  :after (org-clock agile-gtd)
  :custom
  ;; The project list is agile-gtd's, so it is not maintained twice. Records
  ;; rather than bare files, because each consumer wants a different field of
  ;; them: the tag selects clock entries, the name labels the prompt, the file
  ;; base names the CSV.
  (org-clock-projects-projects-function #'agile-gtd-project-records)
  ;; Where the Typst invoice build looks for its CSVs.
  (org-clock-projects-export-directory "~/work/invoice.typ/invoices")
  :config
  (org-clock-projects-mode 1))

(map! :after org-clock-projects
      :map org-mode-map
      :localleader
      :prefix "c"
      :desc "fork clock into this project" "p" #'org-clock-projects-fork
      :desc "clock out ALL projects"       "a" #'org-clock-projects-clock-out-all
      :desc "clear clock project"          "P" #'org-clock-projects-clear)

;; Replaces Doom's `org-clock-goto', degrading to it when fewer than two
;; clocks are open.
(map! :after org-clock-projects
      :leader
      :prefix "n"
      :desc "Switch running org-clock" "o" #'org-clock-projects-switch)

;; The check is the export's dry run: it reports what the export refuses to
;; write over. So the pair sits on one key, the reading half lowercase and the
;; writing half capital, the way Doom's own <leader> n bindings pair.
(map! :after org-clock-projects
      :leader
      :prefix "n"
      :desc "Check project clock data"    "E" #'org-clock-projects-check
      :desc "Export project clock to CSV" "e" #'org-clock-projects-export)

(use-package org-clock-csv
  :after org
  :config
  (defun stfl/org-clock-csv-row-fmt (plist)
    "Return the CSV row for the clock entry PLIST."
    (mapconcat #'identity
               (list (org-clock-csv--escape (plist-get plist ':task))
                     (org-clock-csv--escape (s-join org-clock-csv-headline-separator (plist-get plist ':parents)))
                     (org-clock-csv--escape (org-clock-csv--read-property plist "ARCHIVE_OLPATH")) ; archive_parent
                     (org-clock-csv--escape (plist-get plist ':category))
                     (plist-get plist ':start)
                     (plist-get plist ':end)
                     (plist-get plist ':effort)
                     (plist-get plist ':ishabit)
                     (plist-get plist ':tags)
                     (org-clock-csv--read-property plist "ARCHIVE_ITAGS")
                     (org-clock-csv--read-property plist "AP")
                     (org-clock-csv--read-property plist "TICKET"))
               ","))
  (setq org-clock-csv-header "task,parents,archive_parents,category,start,end,effort,ishabit,tags,archive_tags,ap,ticket"
        org-clock-csv-row-fmt #'stfl/org-clock-csv-row-fmt))

(map! :after org
      :map org-mode-map
      :localleader
      :prefix "d"
      :desc "next-sibling NEXT"          "n" #'agile-gtd-trigger-next-sibling
      :desc "trigger NEXT and block prev" "b" #'agile-gtd-chain-task)

;; TODO keywords are configured in agile-gtd.

(custom-set-faces!
  `(agile-gtd-todo-cancel :foreground ,(doom-blend (doom-color 'red) (doom-color 'base5) 0.35) :inherit (bold org-done))
  `(agile-gtd-todo-idea :foreground ,(doom-darken (doom-color 'green) 0.4) :inherit (bold org-todo)))

(with-eval-after-load 'org
  (setq org-catch-invisible-edits 'error ; Catch invisible edits
        org-track-ordered-property-with-tag t
        org-hierarchical-todo-statistics nil
        ))

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
                      ))

(with-eval-after-load 'org-roam
  (setopt org-roam-directory org-directory
          org-roam-db-location (doom-path doom-local-dir "roam.db")
          ;; Keep hidden files and directories out of org-roam, matching what
          ;; `org-roam-list-files' (fd) skips.
          org-roam-file-exclude-regexp "\\(?:\\`\\|/\\)\\."))

(with-eval-after-load 'org-roam
  (setq +org-roam-open-buffer-on-find-file nil))

(with-eval-after-load 'org-roam-mode
  (add-to-list 'org-roam-mode-sections #'org-roam-unlinked-references-section t))

(with-eval-after-load 'org-roam
  (setq org-roam-dailies-capture-templates
        '(("d" "default"
           entry "* %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n\n"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

;; (use-package websocket
;;     :after org-roam)

;; (use-package org-roam-ui
;;     :after org-roam ;; or :after org
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start nil))

(with-eval-after-load 'org-gcal
;; (use-package org-gcal
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
 (:prefix "n"
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

(use-package ob-mermaid
  :after org
  :config
  (setopt ob-mermaid-default-config-file
          (expand-file-name "mermaid/config.json" doom-user-dir))
  ;; ob-mermaid passes this to the shell unquoted: quote a #rrggbb value.
  (add-to-list 'org-babel-default-header-args:mermaid '(:background-color . "white"))
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

(define-advice org-babel-execute:mermaid (:filter-args (args) stfl/inline-img-files)
  "Embed the local files of `img:' node shapes as data URIs.
Mermaid drops file:// URLs and headless Chromium cannot load a relative
path, so `img: \"icons/x.svg\"' is read relative to the org file instead."
  (let ((re "img: *\"\\([^\":]+\\.\\(svg\\|png\\)\\)\""))
    (cons (replace-regexp-in-string
           re
           (lambda (match)
             (string-match re match)
             (let ((file (expand-file-name (match-string 1 match)))
                   (type (if (equal (match-string 2 match) "svg") "image/svg+xml" "image/png")))
               (format "img: \"data:%s;base64,%s\"" type
                       (base64-encode-string
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally file)
                          (buffer-string))
                        t))))
           (car args) t t)
          (cdr args))))

(define-advice org-babel-execute:mermaid (:after (_body params) stfl/svg-for-librsvg)
  "Adapt a rendered SVG to librsvg, which draws Emacs's inline images.
Mermaid starts each word's <tspan> with a space, which librsvg collapses
unless the root says `xml:space=\"preserve\"'.  It also sets the
background as CSS on the root, which librsvg does not paint, so a rect
of that colour goes underneath."
  (let ((file (cdr (assq :file params))))
    (when (and file (string-suffix-p ".svg" file t) (file-exists-p file))
      (with-temp-file file
        (insert-file-contents file)
        (goto-char (point-min))
        (when (and (re-search-forward "<svg \\([^>]*\\)>" nil t)
                   (not (string-match-p "xml:space=" (match-string 1))))
          (let* ((beg (match-beginning 0))
                 (end (match-end 0))
                 (attrs (match-string 1))
                 (bg (and (string-match "background-color: *\\([^;\"]+\\)" attrs)
                          (match-string 1 attrs)))
                 (box (and (string-match "viewBox=\"\\([^\"]+\\)\"" attrs)
                           (split-string (match-string 1 attrs)))))
            (delete-region beg end)
            (goto-char beg)
            (insert "<svg xml:space=\"preserve\" " attrs ">")
            (when (and bg (= (length box) 4))
              (insert (apply #'format
                             "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"%s\"/>"
                             (append box (list bg)))))))))))

(use-package mermaid-ts-mode
  :mode ("\\.mmd\\'" . mermaid-ts-mode)
  :mode ("\\.mermaid\\'" . mermaid-ts-mode))

(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid-ts)))

(add-transient-hook! #'org-babel-execute-src-block
  (require 'ob-async))

(defvar org-babel-auto-async-languages '()
  "Babel languages which should be executed asyncronously by default.")

(define-advice org-babel-get-src-block-info (:around (orig-fn &optional light datum) stfl/eager-async)
  "Eagarly add an :async parameter to the src information, unless it seems problematic.
This only acts o languages in `org-babel-auto-async-languages'.
Not added when either:
+ session is not \"none\"
+ :sync is set"
  (let ((result (funcall orig-fn light datum)))
    (when (and (string= "none" (cdr (assoc :session (caddr result))))
               (member (car result) org-babel-auto-async-languages)
               (not (assoc :async (caddr result))) ; don't duplicate
               (not (assoc :sync (caddr result))))
      (push '(:async) (caddr result)))
    result))

(with-eval-after-load 'org
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

;; (use-package org-pandoc-import :after org)

(with-eval-after-load 'org-tree-slide (setq org-tree-slide-heading-emphasis nil))

(with-eval-after-load 'org-tree-slide
  (add-hook 'org-tree-slide-play-hook #'doom-disable-line-numbers-h)
  (add-hook 'org-tree-slide-stop-hook #'doom-disable-line-numbers-h))

(with-eval-after-load 'org-tree-slide
  (remove-hook 'org-tree-slide-play-hook #'+org-present-hide-blocks-h)
  (remove-hook 'org-tree-slide-stop-hook #'+org-present-hide-blocks-h))

(use-package orgzly-formatter
  :hook (org-mode . orgzly-formatter-mode))

(with-eval-after-load 'ws-butler
  (add-to-list 'ws-butler-global-exempt-modes 'org-mode))

(use-package ox-hugo :after ox)

(use-package ox-zola
  :after ox
  :config
  (require 'ox-hugo))

(defun stfl/org-agenda-todo-forward ()
  "Move the agenda item at point forward in its TODO keyword sequence."
  (interactive)
  (org-agenda-todo 'right))

(defun stfl/org-agenda-todo-backward ()
  "Move the agenda item at point back in its TODO keyword sequence."
  (interactive)
  (org-agenda-todo 'left))

(map! :after (org org-agenda)
      :map org-agenda-mode-map
      :desc "Prioity up" "C-S-k" #'org-agenda-priority-up
      :desc "Prioity down" "C-S-j" #'org-agenda-priority-down
      :desc "TODO state back" "C-S-h" #'stfl/org-agenda-todo-backward
      :desc "TODO state forward" "C-S-l" #'stfl/org-agenda-todo-forward
      :desc "Narrower view range" "C-M-k" #'agile-gtd-agenda-narrower-range
      :desc "Wider view range" "C-M-j" #'agile-gtd-agenda-wider-range

      :localleader
      "N" #'org-agenda-add-note
      :desc "Filter" "f" #'org-agenda-filter
      :desc "Follow" "F" #'org-agenda-follow-mode
      "o" #'org-agenda-set-property
      "s" #'org-toggle-sticky-agenda

      :prefix ("p" . "Priority and view range")
      :desc "Prioity" "p" #'org-agenda-priority
      :desc "Prioity up" "u" #'org-agenda-priority-up
      :desc "Prioity down" "d" #'org-agenda-priority-down
      :desc "Someday/Maybe toggle" "s" #'agile-gtd-agenda-toggle-someday
      :desc "Add to Someday/Maybe" "S" #'agile-gtd-agenda-set-someday
      :desc "Tickler toggle" "t" #'agile-gtd-agenda-toggle-tickler
      :desc "Add to Tickler" "T" #'agile-gtd-agenda-set-tickler
      :desc "Remove Someday/Maybe" "r" #'agile-gtd-agenda-remove-someday
      :desc "View range" "v" #'agile-gtd-agenda-set-range
      :desc "Narrower view range" "n" #'agile-gtd-agenda-narrower-range
      :desc "Wider view range" "w" #'agile-gtd-agenda-wider-range
      )

(map! :after org-ql
      :map org-ql-view-map
      "z" #'org-ql-view-dispatch)

;; (with-eval-after-load 'org
(setopt
        ;; org-agenda-hide-tags-regexp "\\w+"
        ;; org-agenda-compact-blocks t
        ;; org-agenda-block-separator ?\n
        org-agenda-block-separator ?-
        org-agenda-tags-column 0
        org-agenda-window-setup 'current-window
        ;; org-agenda-todo-ignore-with-date nil
        ;; org-agenda-todo-ignore-deadlines nil
        ;; org-agenda-todo-ignore-timestamp nil
        org-agenda-sticky nil)

(with-eval-after-load 'org-super-agenda
  (with-eval-after-load 'evil-org-agenda
    (setq org-super-agenda-header-map evil-org-agenda-mode-map)))

(defun stfl/org-checkbox-intermediate ()
  "Set the checkbox at point to the intermediate state [-]."
  (interactive)
  (org-toggle-checkbox '(16)))

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Toggle checkbox [-]" "X" #'stfl/org-checkbox-intermediate)

(with-eval-after-load 'org-contrib
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

;; (setopt auth-sources 'password-store)

(use-package age
  :demand t
  :custom
  (age-default-identity "~/.ssh/id_ed25519_stfl")
  (age-default-recipient
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINGSjWr1X80phoVLQDpXyn26SAPytikVdGyTK1ifYxR6 s@stfl.dev")
  :config
  (age-file-enable))

;; (use-package define-word
;;   :after org
;;   :config
;;   (map! :after org
;;         :map org-mode-map
;;         :leader
;;         :desc "Define word at point" "@" #'define-word-at-point))

;; Doom's `:lang org +pandoc' sets `org-pandoc-options' in the ox-pandoc
;; use-package `:init', which runs when ox loads — after this file. A
;; top-level setq here is overwritten, so everything pandoc waits for the
;; package to load first.
(with-eval-after-load 'ox-pandoc
  (setq org-pandoc-options
        '((standalone . t)
          (embed-resources . t)
          (mathjax . t)
          (variable . "revealjs-url=https://revealjs.com")))

  (setq org-pandoc-options-for-typst-pdf
        '((defaults . "~/.local/share/pandoc/defaults/pdf.yaml")))

  (setq org-pandoc-options-for-docx
        '((lua-filter . "table-widths.lua")))

  ;; ox-pandoc bakes its dispatch entries into the backend when it loads, so
  ;; setting `org-pandoc-menu-entry' afterwards changes nothing — the live
  ;; backend has to be amended instead.
  (let ((menu (org-export-backend-menu (org-export-get-backend 'pandoc))))
    (unless (assq ?t (nth 2 menu))
      (setcar (nthcdr 2 menu)
              (append (nth 2 menu)
                      '((?t "to typst-pdf." org-pandoc-export-to-typst-pdf)
                        (?T "to typst-pdf and open."
                            org-pandoc-export-to-typst-pdf-and-open)))))))

(with-eval-after-load 'text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

(with-eval-after-load 'vterm
  (setopt vterm-max-scrollback 200000
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

(with-eval-after-load 'vterm
  (defun vterm-send-return ()
    "Send `C-m' to the libvterm."
    (interactive)
    (deactivate-mark)
    (when vterm--term
      (process-send-string vterm--process "\C-m"))))

(with-eval-after-load 'vterm
  (setopt vterm-tramp-shells '(("docker" "/bin/sh")
                               ("ssh" "/bin/bash"))))

(use-package typst-ts-mode
  :mode ("\\.typ\\'" . typst-ts-mode)
  :config
  (setopt typst-ts-watch-options '("--open")
          typst-ts-indent-offset 2
          typst-ts-enable-raw-blocks-highlight t)
  (map! :map typst-ts-mode-map
        "C-c C-c" #'typst-ts-tmenu
        :localleader
        :desc "Compile" "c" #'typst-ts-compile
        :desc "Watch" "w" #'typst-ts-watch-mode
        :desc "Menu" "m" #'typst-ts-tmenu)
  (add-hook! 'typst-ts-mode-hook #'lsp!))

(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(typst "https://github.com/uben0/tree-sitter-typst")))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(typst-ts-mode . ("tinymist"))))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst") t)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                    :activation-fn (lsp-activate-on "typst")
                    :server-id 'tinymist)))

(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("typst" . typst-ts))

  ;; Set up babel support for Typst
  (org-babel-do-load-languages 'org-babel-load-languages '((typst . t)))

  ;; Configure babel execution for Typst
  (defun org-babel-execute:typst (body params)
    "Execute a block of Typst code with org-babel."
    (message "Executing Typst code block")
    (let* ((in-file (org-babel-temp-file "typst-" ".typ"))
           (out-file (or (cdr (assq :file params))
                         (org-babel-temp-file "typst-" ".pdf")))
           (result-params (cdr (assq :result-params params)))
           (cmdline (or (cdr (assq :cmdline params)) "")))
      (with-temp-file in-file
        (insert body))
      (org-babel-eval
       (format "typst compile %s %s %s" cmdline in-file out-file)
       "")
      (when (member "file" result-params)
        (org-babel-result-cond result-params
          out-file
          (format "[[file:%s]]" out-file))))))

(use-package ox-typst :after ox)

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-space-mode))

(defvar stfl/upload-local-mappings nil
  "Global alist to store local to remote mappings (local-file . remote-path).
Each entry maps an absolute local file path to its corresponding remote path
for ssh-deploy functionality.")

(defun stfl/upload-unregister-all-remotes ()
  "Clear all ssh-deploy mappings and remove buffer-local variables.
Iterates through all stored mappings in stfl/upload-local-mappings and clears
ssh-deploy buffer-local variables for any open buffers, then clears the
global mapping list."
  (interactive)
  ;; For each mapping, find open buffers and clear their local variables
  (dolist (mapping stfl/upload-local-mappings)
    (let* ((local-file (car mapping))
           (buffer (get-file-buffer local-file)))
      (when buffer
        (message "Unregistering ssh-deploy mapping from %s" buffer)
        (with-current-buffer buffer
          (setq-local ssh-deploy-root-local nil
                      ssh-deploy-root-remote nil)))))
  ;; Clear the global alist
  (setq stfl/upload-local-mappings nil))

(defun stfl/upload-register-mapping (&optional remote-path)
  "Register or unregister ssh-deploy mapping for current buffer.
With C-u prefix, unregisters the current buffer's mapping and removes it
from the global stfl/upload-local-mappings list. Otherwise prompts for REMOTE-PATH
and registers the mapping, storing it in both buffer-local variables and the
global mapping list. Updates or replaces any existing mapping for the current file."
  (interactive (if current-prefix-arg
                   (list nil)  ; Don't prompt when unregistering
                 (list (expand-file-name (read-file-name "Remote path: ")))))
  (require 'ssh-deploy)
  (let ((local-file (expand-file-name (buffer-file-name))))
    (if current-prefix-arg
        (progn
          (message "Unregistering ssh-deploy for this buffer")
          (setq-local ssh-deploy-root-local nil
                      ssh-deploy-root-remote nil)
          ;; Remove mapping from global alist
          (setq stfl/upload-local-mappings
                (assoc-delete-all local-file stfl/upload-local-mappings)))
      (progn
        (setq-local ssh-deploy-root-local local-file
                    ssh-deploy-root-remote remote-path)
        (message "registered ssh-deploy for this buffer to %s" ssh-deploy-root-remote)
        ;; Add/update mapping in global alist
        (setq stfl/upload-local-mappings
              (cons (cons local-file remote-path)
                    (assoc-delete-all local-file stfl/upload-local-mappings)))))))

(with-eval-after-load 'ssh-deploy
  (setopt ssh-deploy-async 1))

(map! :map ssh-deploy-menu-map
    :leader
    :prefix "r"
    "l" #'stfl/upload-register-mapping
    "L" #'stfl/upload-unregister-all-remotes)

(when (executable-find "zoxide")
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook (lambda ()
                                 (call-process-shell-command
                                  (format "zoxide add %s"  dired-directory) nil 0))))

  (add-hook 'find-file-hook (lambda ()
                              (call-process-shell-command
                               (format "zoxide add %s"  (file-name-directory buffer-file-name))
                               nil 0)))


  (defun find-file-with-zoxide ()
    (interactive)
    (let ((target (consult--read
		   (process-lines "zoxide" "query" "-l")
		   :prompt "Zoxide: "
		   :require-match nil
		   :lookup #'consult--lookup-member
		   :category 'file
		   :sort nil)))
      (if target
	  (let ((default-directory (concat target "/")))
	    (call-interactively 'find-file))
	(call-interactively 'find-file))))

  (map! :leader :prefix "f" :desc "Find file in zoxide dir" "z" #'find-file-with-zoxide)
  )

(defun consult-dir--zoxide-dirs ()
  "Return zoxide's known directories, most frecent first."
  (mapcar #'file-name-as-directory
          (process-lines "zoxide" "query" "--list")))

(defvar consult-dir--source-zoxide
  `(:name     "Zoxide dirs"
    :narrow   ?z
    :category file
    :face     consult-file
    :history  file-name-history
    :enabled  ,(lambda () (executable-find "zoxide"))
    :items    ,#'consult-dir--zoxide-dirs)
  "Zoxide directory source for `consult-dir'.")

(with-eval-after-load 'consult-dir
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(with-eval-after-load 'flycheck
  (map! :map flycheck-mode-map
        :leader
        "c x" #'consult-flycheck))

(map! :leader ":" #'ielm)

(with-eval-after-load 'lsp-treemacs
  (lsp-treemacs-sync-mode 1))

(map! :after lsp-mode
      :map lsp-mode-map
      :leader
      :prefix "c"
      :desc "Diagnostic for Workspace" "X" #'lsp-treemacs-errors-list)

(with-eval-after-load 'lsp-mode
  (setopt lsp-inlay-hint-enable t
          lsp-headerline-breadcrumb-enable t
          lsp-ui-sideline-enable nil)
  )

(when (executable-find "emacs-lsp-booster")
  (with-eval-after-load 'lsp-mode
    (setopt lsp-use-plists t)

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

(use-package flyover
  :disabled
  :after flycheck
  :config
  (setopt flyover-checkers '(flycheck)
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
       (:prefix "b"
        :desc "Format Buffer" "f" #'+format/buffer)))

(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'php-mode
    (setq lsp-intelephense-licence-key (get-auth-info "intelephense" "ste.lendl@gmail.com")
          lsp-intelephense-files-associations '["*.php" "*.phtml" "*.inc"]
          lsp-intelephense-files-exclude '["**update.php**" "**/js/**" "**/fonts/**" "**/gui/**" "**/upload/**"
                                           "**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**"
                                           "**/node_modules/**" "**/bower_components/**"
                                           "**/vendor/**/{Test,test,Tests,tests}/**"]
          lsp-auto-guess-root nil
          lsp-idle-delay 0.8)))

(with-eval-after-load 'lsp-bridge
  (setopt lsp-bridge-python-multi-lsp-server "basedpyright_ruff"))

(with-eval-after-load 'poetry (setq poetry-tracking-strategy 'projectile))

(with-eval-after-load 'conda (conda-env-autoactivate-mode))

(with-eval-after-load 'projectile
  (projectile-register-project-type 'python-conda '("environment.yml")
                                    :project-file "environment.yml"
                                    :compile "conda build"  ;; does not exist
                                    :test "conda run pytest"
                                    :test-dir "tests"
                                    :test-prefix "test_"
                                    :test-suffix"_test"))

;; (use-package numpydoc
;;   :after python-mode
;;   :commands numpydoc-generate
;;   :config
;;   (map! :map python-mode-map
;;         :localleader
;;         :prefix ("d" . "docstring")
;;         :desc "Generate Docstring" "d" #'numpydoc-generate))

(with-eval-after-load 'ein
  (setopt ein:output-area-inlined-images t
          ein:worksheet-warn-obsolesced-keybinding nil))

(when (modulep! :tools ein)
  (with-eval-after-load 'org
    (require 'ob-ein)))

(set-popup-rule! "^\\*ein:" :ignore t :quit nil)

(with-eval-after-load 'org
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

(map! :mode rustic-mode
      :map rustic-mode-map
      :localleader
      :desc "rerun test" "t r" #'rustic-cargo-test-rerun)

(with-eval-after-load 'rustic
  (when (executable-find "cargo-nextest")
    (setopt rustic-cargo-test-runner 'nextest)))

(with-eval-after-load 'lsp-rust
  (setopt lsp-rust-analyzer-binding-mode-hints t
   ;;        lsp-rust-analyzer-display-chaining-hints t
   ;;        lsp-rust-analyzer-display-closure-return-type-hints t
          lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
   ;;        lsp-rust-analyzer-display-parameter-hints t
   ;;        lsp-rust-analyzer-hide-named-constructor t
          lsp-rust-analyzer-max-inlay-hint-length 40  ;; otherwise some types can get way out of hand
          )
  )

(with-eval-after-load 'eglot
  (setq eglot-workspace-configuration
        (plist-put eglot-workspace-configuration
                   :rust-analyzer
                   '(:inlayHints (:maxLength 40)))))

(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-ts-mode))

(setq-hook! 'nix-ts-mode-hook +format-with 'alejandra)

(add-to-list 'auto-mode-alist '("\\.mq[45h]\\'" . cpp-mode))

;; (use-package gitlab-ci-mode
;;   :mode ".gitlab-ci.yml"
;;   )

;; (use-package gitlab-ci-mode-flycheck
;;   :after flycheck gitlab-ci-mode
;;   :init
;;   (gitlab-ci-mode-flycheck-enable))

(use-package kubernetes
  :disabled
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :disabled
  :after kubernetes)

(use-package kubernetes-helm
  :disabled
  :commands kubernetes-helm-status)

(use-package k8s-mode
  :disabled
  :after yaml-mode
  :hook (k8s-mode . yas-minor-mode))

(use-package sql-indent
  :after sql-mode)

(use-package edbi
  :disabled
  :commands 'edbi:open-db-viewer)

(use-package edbi-minor-mode
  :disabled
  :after sql-mode
  :hook sql-mode-hook)
;; (add-hook 'sql-mode-hook 'edbi-minor-mode)

(use-package exercism-mode
  :disabled
  :after projectile
  :if (executable-find "exercism")
  :commands exercism
  :config (exercism-mode +1)
  :custom (exercism-web-browser-function 'browse-url))

(map! :after rjsx-mode
      :map rjsx-mode-map
      :localleader
      :prefix ("t" . "test")
      "f" #'jest-file
      "t" #'jest-function
      "k" #'jest-file-dwim
      "m" #'jest-repeat
      "p" #'jest-popup)

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-ts-mode))

(add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))

(use-package logview
  :disabled
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

(use-package lsp-ltex
  :disabled
  :after lsp-ltex-active-modes
  :hook (adoc-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-server-store-path "~/.nix-profile/bin/ltex-ls"
        lsp-ltex-version "16.0.0"
        lsp-ltex-mother-tongue "de-AT"
        lsp-ltex-user-rules-path (doom-path doom-user-dir "lsp-ltex")))

(with-eval-after-load 'ispell
  (setopt ispell-personal-dictionary (expand-file-name "ispell/" doom-user-dir)))

(use-package ssh-config-mode :defer t)

(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(bitbake "https://github.com/tree-sitter-grammars/tree-sitter-bitbake")))

(use-package bitbake-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.inc$" . bitbake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.bbclass" . bitbake-ts-mode)))

(with-eval-after-load 'lsp-bridge
  (add-to-list 'lsp-bridge-single-lang-server-mode-list
               ;; '(bitbake-ts-mode . "bitbake-language-server")
               '(bitbake-ts-mode . "language-server-bitbake"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'bitbake-ts-mode-hook t))

(use-package meson-mode
  :disabled
  :config (add-hook! 'meson-mode-hook #'company-mode))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(with-eval-after-load 'lsp-bridge
  (setopt lsp-bridge-c-lsp-server "ccls"))

(with-eval-after-load 'projectile
  (defun run-ctest (arg)
    (interactive "P")
    (let ((projectile-project-test-cmd "cmake --build build && ctest --test-dir build --output-on-failure --rerun-failed"))
      (projectile-test-project arg))))


(map! :mode (c++-mode c++-ts-mode)
      :localleader
      :prefix ("t" . "test")
      :n "t" #'run-ctest
      ;; :n "t" #'gtest-run-at-point
      ;; :n "T" #'gtest-run
      ;; :n "l" #'gtest-list
      )

(use-package turbo-log
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
  (setopt turbo-log-msg-format-template "\"🚀: %s\""
          turbo-log-allow-insert-without-treesit-p t))

(use-package just-mode
  :defer t)

(use-package ztree :disabled)

(with-eval-after-load 'git-commit
  (setq git-commit-summary-max-length 100))

(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk 'all))

(with-eval-after-load 'forge (setq forge-topic-list-columns
                    '(("#" 5 t (:right-align t) number nil)
                      ("Title" 60 t nil title  nil)
                      ("State" 6 t nil state nil)
                      ("Marks" 8 t nil marks nil)
                      ("Labels" 8 t nil labels nil)
                      ("Assignees" 10 t nil assignees nil)
                      ("Updated" 10 t nill updated nil))))

(use-package forge-azure
  :after forge
  :config
  (setq forge-azure-auth 'pat))

(use-package magit-todos
  :after magit
  :config
  (setopt magit-todos-exclude-globs '(".git/" "node_modules/"))
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

(with-eval-after-load 'mu4e
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

(with-eval-after-load 'mu4e-alert
  (setq mu4e-alert-interesting-mail-query
           "flag:unread and not flag:trashed and (m:/gmail/Inbox or m:/gmail/Categories/Updates or m:/pulswerk/INBOX or m:\"/pulswerk/Pulswerk Alle\" or m:/pulswerk/Jira or m:/pulswerk/Gitlab)"))

(with-eval-after-load 'mu4e
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
  (cl-callf append mu4e-header-info-custom
            '((:folder .
               (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
                (lambda (msg)
                  (+mu4e-colorize-str
                   (replace-regexp-in-string "\\`.*/" "" (mu4e-message-field msg :maildir))
                   '+mu4e-header--folder-colors)))))))

(with-eval-after-load 'mu4e
  (setq sendmail-program "/usr/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from") ; , "--read-recipients")
        message-send-mail-function #'message-send-mail-with-sendmail))

;; (use-package mu4e-views
;;   :after mu4e
;;   )

(setq +org-msg-accent-color "#1a5fb4"
      org-msg-greeting-fmt "\nHi %s,\n\n"
      org-msg-signature "\n\n#+begin_signature\n*MfG Stefan Lendl*\n#+end_signature")

(map! :map org-msg-edit-mode-map
      :after org-msg
      :n "G" #'org-msg-goto-body)

(with-eval-after-load 'ediff
  (setq ediff-diff-options "--text"
        ediff-diff3-options "--text"
        ediff-toggle-skip-similar t
        ediff-diff-options "-w"
        ;; ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-floating-control-frame t
        ))

(use-package diffview
  :disabled
  :commands diffview-current
  :config
  (map!
   :after notmuch
   :localleader "d" #'diffview-current))

(use-package blamer
  :commands global-blamer-mode
  :init (map! :leader "t B" #'global-blamer-mode)
  :config
  (map! :leader "g i" #'blamer-show-posframe-commit-info)
  (setopt blamer-idle-time 0.3
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

(defvar stfl/beads-dolt-port nil
  "TCP port of a running Dolt SQL server for `bd', or nil.
Leave nil to let `bd' use the embedded Dolt store under `.beads/'.
Set to a port number only when `.beads' auto-discovery / embedded Dolt
does not work and `bd' must connect to a shared Dolt server instead.")

(use-package beads
  :commands (beads beads-issue-at-point)
  :config
  (when stfl/beads-dolt-port
    (setopt beads-dolt-port stfl/beads-dolt-port))
  ;; beads.el ships no evil integration, and its view buffers are
  ;; read-only Magit-style modes with single-letter keymaps (a c d e g k
  ;; n p q …) that evil normal/motion state shadows.  Open every beads
  ;; view buffer in Emacs state so the keys work without toggling
  ;; holy-mode; the `M-x beads' transient menu is state-agnostic and
  ;; works regardless.
  (when (modulep! :editor evil)
    (defun stfl/beads--evil-emacs-state-h ()
      "Use evil Emacs state in the current beads view buffer."
      (when (string-prefix-p "beads-" (symbol-name major-mode))
        ;; Register the mode so evil keeps choosing Emacs state for later
        ;; buffers, and switch this buffer now if evil is already live here.
        (evil-set-initial-state major-mode 'emacs)
        (when (bound-and-true-p evil-local-mode)
          (evil-emacs-state))))
    ;; Every read-only beads view derives from `special-mode'
    ;; (`tabulated-list-mode' and the dashboard's `vui-mode' both derive
    ;; from it), and `define-derived-mode' runs parent mode hooks — so this
    ;; single hook catches every present and future beads view, fires only
    ;; for special-mode buffers (not every mode change), and structurally
    ;; never touches the `text-mode' editing buffers (compose, prompt-edit).
    (add-hook 'special-mode-hook #'stfl/beads--evil-emacs-state-h)))

(map!
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
       ))

(defun stfl/setup-api-keys ()
  (interactive)
  (message "Setting up API keys")
  (setenv "OPENAI_API_KEY" (password-store-get "API/OpenAI-emacs"))
  (setenv "ANTHROPIC_API_KEY" (password-store-get "API/Claude-emacs"))
  (setenv "GEMINI_API_KEY" (password-store-get "API/Gemini-emacs"))
  (setenv "PERPLEXITYAI_API_KEY" (password-store-get "API/Perplexity-emacs-pro-ste.lendl"))
  (setenv "OPENROUTER_API_KEY" (password-store-get "API/Openrouter-emacs")))

(use-package copilot
  ;; copilot-nes-mode = Next Edit Suggestions; needs copilot-mode in the same
  ;; buffer (NES reuses copilot-mode's language server). TAB accepts / C-g
  ;; dismisses a pending NES edit; those bindings only bind while one is pending.
  :hook ((prog-mode . copilot-mode)
         (prog-mode . copilot-nes-mode))
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

  (setopt copilot-indent-offset-warning-disable t
          copilot-max-char-warning-disable t)

  (setq copilot-lsp-settings '(:github (:copilot (:selectedCompletionModel "gpt-41-copilot"))))

  ;; Use the Nix-provided language server (modules/dev/default.nix:
  ;; llm-agents.copilot-language-server) instead of the npm copy
  ;; copilot-install-server drops into copilot-install-dir. An absolute path
  ;; takes the highest-precedence branch of `copilot-server-executable', so it
  ;; ignores exec-path and never falls back to a self-installed server.
  ;; /run/current-system/sw/bin is a stable symlink that nixos-rebuild updates,
  ;; so this keeps tracking the system package across upgrades.
  (setq copilot-server-executable "/run/current-system/sw/bin/copilot-language-server")
  )

(with-eval-after-load 'gptel
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

  (gptel-make-gh-copilot "Copilot")

  ;; OpenRouter offers an OpenAI compatible API
  (gptel-make-openai "OpenRouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (password-store-get "API/Openrouter-emacs")
    :models '(moonshotai/kimi-k2-thinking))

  ;; Z.ai offers an OpenAI compatible API for GLM models.
  ;; Coding-plan keys MUST use /api/coding/paas/v4 — the general /api/paas/v4
  ;; endpoint returns error 1113 ("insufficient balance") for coding-plan keys.
  (gptel-make-openai "Z.ai"
    :host "api.z.ai"
    :endpoint "/api/coding/paas/v4/chat/completions"
    :stream t
    :key (password-store-get "API/zai")
    :models '(glm-5.1 glm-4.7 glm-5-turbo glm-4.5-air
              (glm-5.1-fast
               :description "GLM 5.1 (thinking disabled)"
               :context-window 200
               :request-params (:model "glm-5.1" :thinking (:type "disabled")))))

  ;; Kimi Code subscription API (requires KimiCLI User-Agent)
  (gptel-make-openai "Kimi"
    :host "api.kimi.com"
    :endpoint "/coding/v1/chat/completions"
    :stream t
    :key (password-store-get "API/moonshot")
    :header (lambda (_info)
              (when-let* ((key (gptel--get-api-key)))
                `(("Authorization" . ,(concat "Bearer " key))
                  ("User-Agent" . "KimiCLI/1.38.0"))))
    :models '((kimi-for-coding
               :description "Kimi K2.6 for coding (thinking enabled)"
               :context-window 256)
              (kimi-for-coding-fast
               :description "Kimi K2.6 for coding (thinking disabled)"
               :context-window 256
               :request-params (:model "kimi-for-coding" :thinking (:type "disabled")))))

  (setopt gptel-default-mode 'org-mode
          ;; gptel-response-prefix-alist '((org-mode . "**** Answer"))
          gptel-api-key (password-store-get "API/OpenAI-emacs")
          ;; gptel-model 'gpt-4o
          gptel-backend (gptel-get-backend "Z.ai")
          gptel-model 'glm-5.1
          gptel-log-level 'info
          ;; gptel-use-curl nil
          gptel-use-curl t
          gptel-stream t)
  )

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :config
  (setq gptel-magit-commit-prompt
        (concat gptel-magit-prompt-conventional-commits
                "\n\n"
                "Repo-specific conventions:\n"
                "- The user message will start with a `Current branch: <name>` line. Inspect that branch name to decide whether a Jira ticket prefix applies.\n"
                "- If the branch matches `<ABC>-<N>-...` or `feature/<ABC>-<N>-...` (case-insensitive — `<N>` is the Jira ticket number), the commit subject MUST start with `<ABC>-<N>: ` followed by the conventional-commits type, e.g. `DRB-123: feat(parser): support nested arrays`.\n"
                "- If the branch does NOT follow this convention, do NOT add any DRB prefix; use the conventional-commits format unchanged."))

  ;; Prepend the current branch to the diff so the LLM can detect a DRB-<N>
  ;; ticket from the branch name. gptel-magit otherwise only sends the diff.
  (defun stfl/gptel-magit--inject-branch (orig-fn callback)
    "Around-advice for `gptel-magit--generate' that prepends branch context."
    (cl-letf* ((orig-output (symbol-function 'magit-git-output))
               ((symbol-function 'magit-git-output)
                (lambda (&rest args)
                  (let ((output (apply orig-output args)))
                    (if (and (stringp (car args)) (string= (car args) "diff"))
                        (concat (format "Current branch: %s\n\n"
                                        (or (magit-get-current-branch) "(detached)"))
                                output)
                      output)))))
      (funcall orig-fn callback)))
  (advice-add 'gptel-magit--generate :around #'stfl/gptel-magit--inject-branch)

  (setq gptel-magit-backend (gptel-get-backend "Z.ai")
        gptel-magit-model 'glm-5.1-fast))
