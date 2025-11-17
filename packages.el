(package! drag-stuff)

(package! org-clock-csv)

(package! org-edna)

;; (package! websocket)
;; (package! org-roam-ui
;;   :recipe (:host github
;;            :repo "org-roam/org-roam-ui"
;;            :files ("*.el" "out")))

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

(package! eat
  :recipe (:type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))))

(package! typst-ts-mode
  :recipe (:type git :host codeberg
           :repo "meow_king/typst-ts-mode"
           :files (:defaults "*.el")))

(package! ox-typst)

(package! pdf-tools :built-in 'prefer)

(package! flyover :recipe (:host github :repo "konrad1977/flyover"))

;; (package! numpydoc)

(package! kubernetes :disable t)
(package! kubernetes-evil :disable t)
(package! kubernetes-helm :disable t)
(package! k8s-mode :disable t)

(package! sql-indent)

(package! edbi :disable t)
(package! edbi-minor-mode :disable t)

(package! exercism-mode
  :disable t
  :recipe (:host github
           :repo "timotheosh/exercism-mode"))

(package! jest :disable t)

(package! logview :disable t)

(package! lsp-ltex :disable t)

;; bbatsov/adoc-mode
(package! adoc-mode)

(package! ssh-config-mode)

(package! bitbake-ts-mode)

(package! meson-mode :disable t)

(package! turbo-log
  :recipe (:host github
           :repo "artawower/turbo-log"))

(package! just-mode)

(package! ztree :disable t)

(package! magit-todos)

;; (package! mailscripts.el
;;   :recipe (:host github :repo "spwhitton/mailscripts" :files ("mailscripts.el")))

(package! diffview :disable t)

(package! blamer)

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

(package! claude-code-ide
  :recipe (:host github
           :repo "manzaltu/claude-code-ide.el"))
