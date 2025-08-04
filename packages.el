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

;; (package! lsp-bridge
;;   :recipe (:host github
;;            :repo "manateelazycat/lsp-bridge"
;;            :branch "master"
;;            :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;            ;; do not perform byte compilation or native compilation for lsp-bridge
;;            :build (:not compile)))

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

(package! ssh-config-mode
  ;; :recipe (:host github
           ;; :repo "jhgorrell/ssh-config-mode-el"
           ;; :files ("*.el" "out"))
  )

(package! bitbake-ts-mode
  :recipe (:host github
           :repo "seokbeomKim/bitbake-ts-mode"
           :files ("*.el")))

;; (package! bitbake-modes
;;   :recipe (:host bitbucket
;;            :repo "olanilsson/bitbake-modes"
;;            ;; :files ("*.el")
;;            ))

;; (package! bb-mode
;;   :recipe (:host github
;;            :repo "mferland/bb-mode"
;;            :files ("*.el")))

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
