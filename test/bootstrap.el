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
;; Initialize as interactive: registers doom-start machinery and profile-init advice
(doom-initialize t)
;; Load the profile init (defines doom-startup, module autoloads, load-path)
(doom-load (doom-profile-init-file doom-profile) t)
;; Run doom-startup: loads all module configs and user config.el
(doom-startup)
;; Trigger doom-after-init-hook (incremental package loading etc.)
(doom-finalize)
