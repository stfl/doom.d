;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Stefan Lendl"
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 13)
      ;; doom-variable-pitch-font (font-spec :family "Fira Code")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Code Medium" :size 20))
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen) ;; start full-screen

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;;;;;;;;; Org Mode

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-roam-directory "~/Dropbox/org/roam")
(use-package deft
      :after org
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "~/Dropbox/org/roam"))

(after! org
  (setq! org-startup-with-inline-images t))

;; (map! (:map dired-mode-map
;;     :desc "org-hug export all"
;;     :n "C-8" (lambda()
;;                 (interactive)
;;                 (diredp-do-apply-function 'org-hugo-export-wim-to-md '(4)))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq which-key-idle-delay 0.3)

(after! projectile
  ;; (setq projectile-project-search-path
  ;;       (cddr (directory-files "/work" t))) ;;add all dirs inside ~/work -> https://github.com/bbatsov/projectile/issues/1500
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  )

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(after! evil-snipe
  (setq evil-snipe-scope 'buffer)
  )

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

(map! (:map lsp-mode-map
       :desc "Diagnostic for Workspace"
       :leader
       :n "c X" #'lsp-treemacs-errors-list))

;; dir tree diff
(use-package! ztree)

(use-package! fira-code-mode
  :after prog-mode
  ;; :config (global-fira-code-mode)
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" "*" "+" ":")) ;; List of ligatures to turn off
  )

;; display more columns in forge list topic
(after! forge (setq forge-topic-list-columns
                    '(("#" 5 t (:right-align t) number nil)
                      ("Title" 60 t nil title  nil)
                      ("State" 6 t nil state nil)
                      ("Marks" 8 t nil marks nil)
                      ("Labels" 8 t nil labels nil)
                      ("Assignees" 10 t nil assignees nil)
                      ("Updated" 10 t nill updated nil))))


(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(after! (lsp-mode php-mode)
  (setq lsp-intelephense-files-associations '["*.php" "*.phtml" "*.inc"])
  (setq lsp-intelephense-files-exclude '["**update.php**" "**/js/**" "**/fonts/**" "**/gui/**" "**/upload/**"
                                         "**/.git/**" "**/.svn/**" "**/.hg/**" "**/CVS/**" "**/.DS_Store/**" "**/node_modules/**" "**/bower_components/**" "**/vendor/**/{Test,test,Tests,tests}/**"])
  (setq lsp-intelephense-licence-key
        "00VUH296RB9W4S0") ;;(get-string-from-file "~/.doom.d/intelephense.txt"))
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

;; (after! conda
;;   (setq conda-anaconda-home "/opt/anaconda"
;;         conda-env-home-directory "/opt/anaconda"
;;    )
;;   )

(after! undo-fu-mode
  (setq undo-limit         1000000
        undo-strong-limit  8000000
        undo-outer-limit   8000000)
  )

(after! dired
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _?_ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("?" nil :color blue))
  (map! (:map dired-mode-map
        :desc "Dired Hydra"
        :n "?" 'hydra-dired/body))
  ;; (define-key dired-mode-map "?" 'hydra-dired/body)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fira-code-mode-disabled-ligatures (quote ("[]" "#{" "#(" "#_" "#_(" "x" "*" "+" ":")))


 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
