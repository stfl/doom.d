# Local package development

How `agile-gtd` and `org-mcp` are wired into Doom from local checkouts, and how
to switch either between local and published sources.

- `agile-gtd` is developed at `~/work/agile-gtd` and `org-mcp` at `~/work/org-mcp`.
- Both are wired into Doom via `package!` `:local-repo` recipes in `config.org` (in the `** agile-gtd` and `** org-mcp` sections).
- straight.el symlinks their `.el` files into the build directory, so `doom emacs --batch` finds them automatically without any manual load-path setup.
- `:build (:not compile)` is set for both — edits to `.el` files in the local repos are live on the next Emacs session (or `eval-buffer`) without rerunning `doom sync`.
- To switch a package from local dev to the published GitHub version (or vice-versa), swap the commented/uncommented `package!` line in `config.org`, run `doom +org tangle config.org`, then `doom sync -u`.
  - Local dev active: `:recipe (:local-repo "~/work/<pkg>" :build (:not compile))` is uncommented, GitHub line is commented out.
  - GitHub version active: `:recipe (:host github :repo "...")` is uncommented, local-repo line is commented out.

