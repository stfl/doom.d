(package! drag-stuff)

;; Local dev — switch to GitHub recipe once published:
(package! agile-gtd
  ;; :recipe (:local-repo "~/work/agile-gtd" :build (:not compile)))
  :recipe (:host github :repo "stfl/agile-gtd.el"))

(package! mcp-server-lib)
(package! org-mcp
  :recipe (:host github :repo "stfl/org-mcp"
           :files (:defaults "org-mcp-stdio.sh")))
  ;; :recipe  (:local-repo "~/work/org-mcp" :build (:not compile)))

;; Swap the two recipes to develop against the local checkout.
(package! org-clock-projects
  ;; :recipe (:local-repo "~/work/org-clock-projects" :build (:not compile)))
  :recipe (:host github :repo "stfl/org-clock-projects"))
;; org-clock-projects requires org-clock-csv for the export, so the dependency
;; is pinned beside what depends on it; the row format it writes through is
;; configured under "org-clock-csv row format" below.
(package! org-clock-csv)

(package! org-edna)

;; (package! websocket)
;; (package! org-roam-ui
;;   :recipe (:host github
;;            :repo "org-roam/org-roam-ui"
;;            :files ("*.el" "out")))

(package! ob-mermaid
  :disable t)

;; (package! org-pandoc-import
;;   :recipe (:host github
;;            :repo "tecosaur/org-pandoc-import"
;;            :files ("*.el" "filters" "preprocessors")))

(package! orgzly-formatter
  :recipe (:host github :repo "stfl/orgzly-formatter.el"))

(package! ox-hugo)

(package! ox-zola
  :recipe (:host github :repo "gicrisf/ox-zola"))

(package! org-super-agenda)

(package! org-ql)

(package! age
  :recipe (:host github :repo "anticomputer/age.el"))

(package! vterm :disable t)

(package! typst-ts-mode
  :recipe (:type git :host codeberg
           :repo "meow_king/typst-ts-mode"
           :files (:defaults "*.el")
           ;; TEMPORARY WORKAROUND — tracked as stfl/doom.d#2.
           ;;
           ;; Emacs 31's `loaddefs-generate' copies the `define-compilation-mode'
           ;; form into the autoloads file verbatim rather than reducing it to an
           ;; autoload stub, so loading the autoloads fails with a void
           ;; `define-compilation-mode' before `compile' is available. Emit an
           ;; explicit stub instead.
           ;;
           ;; Upstream is on Codeberg and the bug is reported but unfixed:
           ;;   https://codeberg.org/meow_king/typst-ts-mode/issues/103
           :pre-build ("perl" "-0pi" "-e"
                       "s/;;;###autoload\\n\\(define-compilation-mode ([^\\s()]+)/;;;###autoload (autoload \\x27$1 \\x22typst-ts-compile\\x22 nil t)\\n(define-compilation-mode $1/g"
                       "typst-ts-compile.el")))

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

(package! forge-azure
  :recipe (:host github
           :repo "dakra/forge-azure"))

(package! magit-todos)

;; (package! mailscripts.el
;;   :recipe (:host github :repo "spwhitton/mailscripts" :files ("mailscripts.el")))

(package! diffview :disable t)

(package! blamer)

;; TEMPORARY FORK — revert to r0man/beads.el once PR #66 lands.
;; Tracked as stfl/doom.d#1.
;;
;; On Emacs 31, loading beads-autoloads.el fails with a void
;; `transient-define-prefix'. `beads' and `beads-more-menu' carry a bare
;; `;;;###autoload' cookie on their `transient-define-prefix' form, and
;; Emacs 31 only reduces such a form to an autoload stub when the defining
;; macro is *loaded* while autoloads are generated — straight merely
;; autoloads transient, so `loaddefs-generate' copies the raw form instead.
;; The fork emits explicit autoload stubs, matching the idiom beads already
;; uses in its other command files.
;;
;;   PR: https://github.com/r0man/beads.el/pull/66
(package! beads
  :recipe (:host github :repo "stfl/beads.el"
           :branch "fix/emacs31-transient-autoloads"
           :files ("lisp/*.el"))
  :pin "bc78afb38483b1edbdd1953c81384fffa56f544a")

(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))
