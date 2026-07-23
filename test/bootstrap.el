;;; bootstrap.el --- Load the full Doom config in a batch sandbox -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs -q --batch -l ~/.config/doom/test/bootstrap.el -l /path/to/test.el 2>/dev/null
;;
;; After this file loads, (require 'org) fires all eval-after-load 'org hooks
;; (agile-gtd, org-mcp, etc.) exactly as in interactive Emacs.
;;
;; WARNING: do NOT set DOOMPROFILE in the environment before running.
;; When DOOMPROFILE is set, doom-data-dir switches to ~/.local/share/doom/
;; and doom-profile-init-file resolves to a non-existent path.

(setq user-emacs-directory (expand-file-name "~/.config/emacs/"))

;; Loads doom.el: sets doom-user-dir, doom-profile=nil, doom-data-dir → .local/etc/
(load (expand-file-name "lisp/doom" user-emacs-directory) nil t nil 'must-suffix)

;; Registers the `doom-profile' cl-defstruct. `doom--profile' normalizes a
;; profile struct into a (NAME . REF) key only when `cl-struct-p' recognizes it;
;; without the struct class registered it hands the struct through and the
;; directory helpers fail on (car PROFILE).
(require 'doom-profiles)

;; `noninteractive' must be nil for the duration of startup. The generated
;; profile init file guards its module-context cache with
;; (static-unless noninteractive ...) — the setplist forms that `modulep!'
;; consults to resolve bare flags such as (modulep! +keybinds). That macro is
;; expanded when the init file is loaded, so with `noninteractive' non-nil the
;; cache is omitted, every bare `modulep!' returns nil, and modules silently
;; skip parts of themselves — :doom compat never loads +keybinds.el, leaving
;; `map!' and `define-key!' undefined.
;;
;; doom-initialize's first argument is the profile id; nil selects the default
;; profile. It also loads the profile init file, which defines `doom-startup'.
(let ((noninteractive nil))
  (doom-initialize nil t)
  ;; Loads all module configs and user config.el
  (doom-startup))

;; Trigger doom-after-init-hook (incremental package loading etc.)
(doom-finalize)
