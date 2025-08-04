(setq user-full-name "Stefan Lendl"
      user-mail-address "ste.lendl@gmail.com")

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(defun stfl/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.or
#+begin_src elisp
(use-package! lsp-bridge
  :config
  (setq! lsp-bridge-user-langserver-dir (doom-path doom-private-dir "langserver")
         lsp-bridge-enable-inlay-hint t
         lsp-bridge-enable-hover-diagnostic t
         ;; lsp-bridge-enable-signature-help t
         ;; lsp-bridge-enable-auto-format-code nil
         ;; lsp-bridge-enable-org-babel t
         lsp-bridge-log-level 'default
         ;; acm-enable-capf t
         )
  ;; (map! :map ?? lsp-bridge-map)
  (global-lsp-bridge-mode))

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

(map!
 :mode rustic-mode
 :map rustic-mode-map
 :localleader
 :desc "rerun test" "t r" #'rustic-cargo-test-rerun
 )

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

(use-package! bitbake-ts-mode
  :config (add-to-list 'auto-mode-alist '("\\.inc$" . bitbake-ts-mode)))

(after! lsp-bridge
  (add-to-list 'lsp-bridge-single-lang-server-mode-list
               ;; '(bitbake-ts-mode . "bitbake-language-server")
               '(bitbake-ts-mode . "language-server-bitbake")))

;; (use-package! bitbake-modes
;;   :config (add-to-list 'auto-mode-alist '("\\.inc$" . bitbake-mode))
;;   :defer t)

;; (use-package bb-modes
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.bb" . bb-mode))
;;   (add-to-list 'auto-mode-alist '("\\.bbappend" . bb-mode))
;;   (add-to-list 'auto-mode-alist '("\\.bbclass" . bb-mode))
;;   (add-to-list 'auto-mode-alist '("\\conf/.conf" . bb-mode))
;;   (add-to-list 'auto-mode-alist '("\\.inc$" . bb-mode))
;;   :config
;;   (bitbake-lsp-register))

;; (defun bitbake-lsp-register ()
;;   "Register to start using this language server."
;;   (interactive)
;;   (add-to-list 'lsp-language-id-configuration '(bb-mode . "bitbake"))
;;     (lsp-register-client
;;      (make-lsp-client :new-connection
;;                       (lsp-stdio-connection `(,(executable-find "bitbake-language-server")))
;;                       :activation-fn (lsp-activate-on "*.bb" "*.bbappend" "*.bbclass" "*.inc" "conf/*.conf")
;;                       :server-id 'bitbake)))

(use-package! meson-mode
  :config (add-hook! 'meson-mode-hook #'company-mode))

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(defun run-ctest (arg)
  (interactive "P")
  (let ((projectile-project-test-cmd "cmake --build build && ctest --test-dir build --output-on-failure --rerun-failed"))
    (projectile-test-project arg)))


(map! ;;:after cc-mode
      :mode c++-mode
      :map c++-mode-map
      :localleader 
      :prefix ("t" "test")
      :n "t" #'run-ctest
      ;; :n "t" #'gtest-run-at-point
      ;; :n "T" #'gtest-run
      ;; :n "l" #'gtest-list
      )

;; (after! lsp-mode
;;   (set-lsp-priority! 'ccls 2))

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

(use-package just-mode)

(use-package justl
  :disabled
  )
  ;; :config
  ;; (map! :n "e" 'justl-exec-recipe))

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
         blamer-type 'visual
         blamer-max-commit-message-length 80
         ;; blamer-max-lines 100
         ;; blamer-type 'posframe-popup
         ;; blamer-type 'overlay-popup
         blamer-min-offset 40)
  ;; (add-hook! org-mode-hook (λ! (blamer-mode 0)))
  )

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
