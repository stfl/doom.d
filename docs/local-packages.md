# Local package development

How `agile-gtd` and `org-records-mcp` reach Doom, and how to switch either
between a local checkout and the published source.

- `agile-gtd` is developed at `~/work/agile-gtd` and `org-records-mcp` at `~/work/org-records-mcp`.
- `agile-gtd` comes from its local checkout through a `package!` `:local-repo` recipe in `config.org` (the `** agile-gtd` section). `org-records-mcp` comes from GitHub (`stfl/org-records-mcp`, default branch), in the `** org-records-mcp` section.
- straight.el symlinks a local checkout's `.el` files into the build directory, so `doom emacs --batch` finds them automatically without any manual load-path setup.
- `:build (:not compile)` on a local recipe makes edits to its `.el` files live on the next Emacs session (or `eval-buffer`) without rerunning `doom sync`.
- To switch a package between a local checkout and GitHub, swap the commented/uncommented `package!` recipe in `config.org`, then run `doom sync`, which tangles `config.el` and `packages.el` from `config.org` and rebuilds.
  - Local checkout: `:recipe (:local-repo "~/work/<pkg>" :build (:not compile))`.
  - GitHub: `:recipe (:host github :repo "...")`.
- To take a new commit of one GitHub package, pull that package alone, then run `doom sync`: `git -C ~/.config/emacs/.local/straight/repos/<pkg> pull`, or `M-x straight-pull-package`. `doom sync -u` pulls every package.
