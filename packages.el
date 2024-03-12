(package! demap :recipe (:host gitlab :repo "sawyerjgardner/demap.el"))
;; (package! demap)

(package! org :pin "806abc5a2bbcb5f884467a0145547221ba09eb59")

(package! org-super-agenda)

(package! org-ql)

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

(when (version<= "29" emacs-version)
  (package! emacsql-sqlite-builtin))

(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))

(package! numpydoc)

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

;; bbatsov/adoc-mode
(package! adoc-mode)

;; (package! jinx)

(package! lsp-ltex
  :disable t
  )

(package! ztree)

;; (package! mailscripts.el
;;   :recipe (:host github :repo "spwhitton/mailscripts" :files ("mailscripts.el")))

(package! diffview)

(package! edit-server)

(package! gptel)
