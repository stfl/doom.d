(package! drag-stuff)

(package! org-clock-csv)

(package! org-edna)

(package! websocket)
(package! org-roam-ui
  :recipe (:host github
           :repo "org-roam/org-roam-ui"
           :files ("*.el" "out")))

(package! ob-mermaid
  :disable t)

(package! org-jira
  :disable t)

;; (package! org-pandoc-import
;;   :recipe (:host github
;;            :repo "tecosaur/org-pandoc-import"
;;            :files ("*.el" "filters" "preprocessors")))

(package! org-super-agenda)

(package! org-ql)

(package! vterm :disable t)

(package! typst-ts-mode
  :recipe (:type git :host codeberg
           :repo "meow_king/typst-ts-mode"
           :files (:defaults "*.el")))

(package! lsp-mode :disable t)

(package! flyover :recipe (:host github :repo "konrad1977/flyover"))

(use-package! flyover
  :after flycheck
  :config
  (setq flyover-levels '(error warning info))  ; Show all levels
  (setq flyover-use-theme-colors t) ;; Use theme colors for error/warning/info faces
  ;; (setq flyover-background-lightness 45) ;; Adjust background lightness (lower values = darker)
  ;; (setq flyover-percent-darker 40) ;; Make icon background darker than foreground
  ;; (setq flyover-text-tint 'lighter) ;; or 'darker or nil
  ;; (setq flyover-text-tint-percent 50) ;; "Percentage to lighten or darken the text when tinting is enabled."


  (setq flyover-debug nil) ;; Enable debug messages
  
  ;; (setq flyover-debounce-interval 0.2) ;; Time in seconds to wait before checking and displaying errors after a change

  ;; Enable wrapping of long error messages across multiple lines
  ;; (setq flyover-wrap-messages t)

  ;; Maximum length of each line when wrapping messages
  ;; (setq flyover-max-line-length 80)


  (add-hook 'flycheck-mode-hook #'flyover-mode)
  )

;; (package! numpydoc)

(package! kubernetes :disable t)
(package! kubernetes-evil :disable t)
(package! kubernetes-helm :disable t)
(package! k8s-mode :disable t)

(package! sql-indent)

(package! edbi :disable t)
(package! edbi-minor-mode :disable t)

(package! exercism-mode :recipe (:host github :repo "timotheosh/exercism-mode") :disable t)

(package! jest)

(package! logview :disable t)

(package! lsp-ltex
  :disable t
  )

;; bbatsov/adoc-mode
(package! adoc-mode)

(package! ssh-config-mode)

(package! bitbake-ts-mode)

(package! meson-mode :disable t)

;; (package! gtest-mode
;;   :recipe (:host github
;;            :repo "ppatoria/elisp"
;;            :files ("gtest-mode/*.el")))

(package! turbo-log :recipe (:host github :repo "artawower/turbo-log"))

(package! just-mode)

(package! justl :recipe (:host github :repo "psibi/justl.el") :disable t)

(package! ztree :disable t)

(package! magit-todos :disable t)

;; (package! mailscripts.el
;;   :recipe (:host github :repo "spwhitton/mailscripts" :files ("mailscripts.el")))

(package! diffview :disable t)

(package! blamer :disable t)

(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))

(package! copilot-chat
  :recipe (:host github
           :repo "chep/copilot-chat.el"
           :files ("*.el")))

(package! codeium
  :recipe (:host github
           :repo "Exafunction/codeium.el")
  :disable t)

;; (package! gptel)

(package! aidermacs
    :recipe (:host github
             :repo "MatthewZMD/aidermacs"
             :files ("*.el")))
