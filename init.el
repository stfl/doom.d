                                        ; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;; (company          ; the ultimate code completion backend
       ;;  +auto            ; as-you-type code completion
       ;;  +childframe)     ; a nicer company UI (Emacs 26+ only)
       (corfu
        +icons
        +orderless
        ;; +dabbrev
        )
       ;; helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; (ivy              ; a search engine for love and life
       ;; +fuzzy
       ;; +icons
       ;; +childframe
       ;; )
       (vertico
        +icons
        ;; +childframe
        )

       :ui
       ;; deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;; (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
                                        ; (:cond ((unless (string= system-name "stefan-t3600")
       indent-guides     ; highlighted indent columns
                                        ; )))
       (smooth-scroll     ; smooth scrolling
        +interpolate)
       ;; minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;; (ligatures         ; replace bits of code with pretty symbols
       ;;  ;; +extra
       ;;  )
       ;; +fira)
       ;; tabs              ; an tab bar for Emacs
       (treemacs          ; a project drawer, like neotree but cooler
        +lsp)            ; lsp-treemacs
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       ;; vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format          ; automated prettiness
        +onsave
        ;; +lsp
        )
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;; parinfer          ; turn lisp into python, sort of
       ;; rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired             ; making dired pretty [functional]
        +icons
        ;; +dirvish         ; ranger-like interface
        )
       ;; electric          ; smarter, keyword-based electric-indent
       (ibuffer
        +icons)           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree
       (undo
        ;; +tree
        )

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs
       vterm             ; another terminals in Emacs

       :checkers
       (syntax              ; tasing you for every semicolon you forget
        +childframe
        ;; +flymake        ; use flymake instead of flycheck
        +icons)
       ;; (spell             ; tasing you for misspelling mispelling
       ;;  +aspell
       ;;  +enchant
       ;;  +everywhere
       ;; )
       ;; grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       (debugger          ; FIXME stepping through code, to help you add bugs
        +lsp)
       direnv
       (docker
        +lsp)
       editorconfig      ; let someone else argue about tabs vs spaces
       ;; ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets         ; ...or in Dash docsets locally
        +dictionary
        +offline
        )
       ;; +offline)
       (lsp +peek)
       ;;macos             ; MacOS-specific commands
       (magit             ; a git porcelain for Emacs
        +forge)
       ;;make              ; run make tasks from Emacs
       (pass +auth)        ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       tree-sitter
       llm

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc                 ; C/C++/Obj-C madness
        +lsp
        +tree-sitter)
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;go                ; the hipster dialect
       ;; (haskell            ; a language that's lazier than I am
       ;;  +dante
       ;;  +ghcide)
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript         ; all(hope(abandon(ye(who(enter(here))))))
        +lsp
        +tree-sitter)
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       ;; (lua               ; one-based indices? one-based indices
       ;;  +lsp
       ;;  +fennel)
       (markdown)          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix               ; I hereby declare "nix geht mehr!"
        +tree-sitter
        +lsp)
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        ;; +hugo            ; use Emacs for hugo blogging
        +pretty
        ;; +passwords    ; org-passwords
        ;; +crypt        ; org-crypt
        ;; +jupyter        ; ipython/jupyter support for babel
        ;; +contacts     ; Enable [[doom-package:org-contacts]] integration.
        +pandoc          ; export-with-pandoc support
        ;; +pomodoro        ; be fruitful with the tomato technique
        +roam2
        ;; +gnuplot
        ;; +present      ; using org-mode for presentations
        )
       ;; raku              ; write code no one else can comprehend
       (php                ; perl's insecure younger brother
        +lsp
        +tree-sitter)
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python             ; beautiful is better than ugly
        ;; +pyenv
        +poetry
        +lsp
        +pyright           ; NOTE can use basedpyright
        ;; +conda
        +tree-sitter)
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;; (rest              ; Emacs as a REST client
       ;;  +jq)
       ;;rst               ; ReST in peace
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust               ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        +lsp
        +tree-sitter)
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       (sh                 ; she sells {ba,z,fi}sh shells on the C xor
        +lsp
        +tree-sitter)
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web                ; the tubes
        +lsp
        +tree-sitter)
       yaml                ; JSON, but readable

       :email
       ;; (mu4e +gmail +org)
       ;; (notmuch
       ;;  +org              ; Enable [[doom-package:org-mime]] for writing HTML emails using org-mode.
       ;;  +afew             ; Enable integration with [[https://github.com/afewmail/afew][afew]].
       ;;  )
       ;;(wanderlust +gmail)

       :app
       ;; calendar                   ; A dated approach to timetabling
       ;;emms                       ; Multimedia in Emacs is music to my ears
       ;;irc               ; how neckbeards socialize
       ;; everywhere                   ; *leave* Emacs!? You must be joking.
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       literate
       (default
        +bindings
        +smartparens)
       )
