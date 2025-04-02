(package! demap :recipe (:host gitlab :repo "sawyerjgardner/demap.el"))
;; (package! demap)

(package! org-clock-csv)

(package! org-edna)

;; (package! org-modern)

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

(package! pdf-tools :built-in 'prefer)

;; (package! numpydoc)

(package! kubernetes)
(package! kubernetes-evil)
(package! kubernetes-helm)
(package! k8s-mode)

(package! sql-indent)

(package! edbi)
(package! edbi-minor-mode)

(package! exercism-mode :recipe (:host github :repo "timotheosh/exercism-mode"))

(package! jest)

(package! logview)

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

;; (package! bitbake-modes
;;   :recipe (:host bitbucket
;;            :repo "olanilsson/bitbake-modes"
;;            ;; :files ("*.el")
;;            ))

(package! meson-mode)

;; (package! gtest-mode
;;   :recipe (:host github
;;            :repo "ppatoria/elisp"
;;            :files ("gtest-mode/*.el")))

(package! turbo-log :recipe (:host github :repo "artawower/turbo-log"))

(package! just-mode)

(package! justl :recipe (:host github :repo "psibi/justl.el"))

(package! ztree)

(package! magit-todos)

;; (package! mailscripts.el
;;   :recipe (:host github :repo "spwhitton/mailscripts" :files ("mailscripts.el")))

(package! diffview)

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
           :repo "Exafunction/codeium.el"))

(package! gptel)

(package! elysium)

(package! aider
  :recipe (:host github
           :repo "tninja/aider.el"
           :files ("*.el")))

(package! aidermacs)
