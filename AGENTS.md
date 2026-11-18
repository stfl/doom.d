# AGENTS.md

Personal Doom Emacs configuration in Emacs Lisp and Org. Prefer small changes that fit Doom idioms.

Commands and procedures live in `docs/`:

| Page | Answers |
|---|---|
| [`docs/running-and-testing.md`](docs/running-and-testing.md) | How do I tangle, sync, lint, and run the test harnesses? |
| [`docs/batch-emacs.md`](docs/batch-emacs.md) | How do I query a live Emacs, or load the config in batch? |
| [`docs/debugging.md`](docs/debugging.md) | The config won't load — what do I run? |
| [`docs/local-packages.md`](docs/local-packages.md) | Where do `agile-gtd` and `org-records-mcp` come from, and how do I switch them between local and GitHub? |

A new fact earns a place in this file only by constraining how code is changed.
A command, a procedure, or a troubleshooting recipe goes to `docs/` and is
linked from the table above.

## Source of truth

- `config.org` is the source of truth for configuration; `config.el` and `packages.el` are tangled from it. Edit `config.org`, never `config.el`.
- `packages.el` comes from the `package!` blocks in `config.org`. `langserver/*.json` is generated when a matching Org block exists.
- `init.el` (Doom modules), `my-deft-title.el` and snippets are handwritten; edit them directly.
- `custom.el` is Emacs Custom output; leave it alone.
- Automatic literate recompilation is disabled, so nothing tangles for you. After editing `config.org`, `init.el` or a `package!` block, run `~/.config/emacs/bin/doom sync`; it regenerates `config.el`. `doom +org tangle config.org` tangles only blocks with an explicit `:tangle` target and does not touch `config.el`.
- Use `doom sync --rebuild` only when package state, the Emacs version or stale compilation looks suspect.
- Commit `config.org`, `config.el` and `packages.el` together, after `doom sync`; never `config.org` alone.
- Mention regenerated files in the handoff.

## Own packages: agile-gtd and org-records-mcp

- Both are the user's own packages, not third-party code. `agile-gtd` lives at `~/work/agile-gtd` (GitHub `stfl/agile-gtd.el`), `org-records-mcp` at `~/work/org-records-mcp` (GitHub `stfl/org-records-mcp`).
- When a fix or feature falls inside what one of them does, make it in that package's repo and follow its `AGENTS.md`. Do not work around it in `config.org` with advice, overrides or copied code.
- `agile-gtd` owns the GTD workflow: TODO keywords, priorities and rank, agenda commands and view ranges, capture and refile, the Org settings it applies, its org-records-mcp views, and how `org-store-link` picks an identifier and copies links.
- `org-records-mcp` owns the MCP server: its tools, link minting and resolution, and its file scope.
- `config.org` keeps only the user's settings for them: the project list, extra agenda files, enabling, and starting the MCP server.
- Doom installs both from GitHub, so a package change reaches this config only after it is pushed and the package is pulled, or once the local recipe is switched on; see [`docs/local-packages.md`](docs/local-packages.md). Say which one applies in the handoff, and do not push a package repo without the user's go.

## Code conventions

- Dependency sources: read the straight checkouts under `~/.config/emacs/.local/straight/repos/*`.
- Use plain `with-eval-after-load`, `use-package` and `setopt` in private config, following Doom upstream guidance. Do not use `after!`, `use-package!` or `setq!`.
- Put package-specific configuration inside `with-eval-after-load` or `use-package`. A block that waits on two features nests two `with-eval-after-load` forms. Use `require` only when eager loading is needed.
- Doom macros without a vanilla replacement (`map!`, `add-hook!`, `setq-hook!`, `custom-set-faces!`, `set-*!`) remain the idiom; do not replace them.
- Write one-off advice with `define-advice`, named `stfl/<what>` so the function becomes `TARGET@stfl/<what>`. Keep `defadvice!` or `defun` plus `advice-add` for advice another form calls or removes by name.
- Add packages in tangled `package!` blocks, never as runtime installs. A disabled package is `:disable t` in `packages.el` and also `:disabled` in its `use-package` block, because plain `use-package` does not skip packages that `package!` disabled.
- Prefix repo-specific functions and variables with `stfl/`; keep borrowed prefixes such as `my-deft/` and `ibizaman/`.
- Keep lexical binding where a file enables it. Match surrounding whitespace and quote style; do not reformat unrelated code.
- Org code depends on agenda markers, headline context and inherited properties; watch the interactions with `org-agenda`, `org-roam`, `org-ql` and `org-super-agenda` when changing it.
- Byte-compile or batch-load a handwritten helper you touched. Smoke-test an interactive change in Doom.

## Issue tracking

- Issues for this repo are GitHub issues on `stfl/doom.d`: `gh issue create --repo stfl/doom.d ...`. Do not track this repo's work in the `dotfiles` beads database or any other repo's tracker.
- The split is by subject: Doom, Emacs, Org and package work goes to `stfl/doom.d`; NixOS, Home Manager, host and system-level work stays in the `dotfiles` beads database (`bd create` from `~/.config/dotfiles`).
- Work on the code of `agile-gtd` or `org-records-mcp` goes to that package's own issues (`stfl/agile-gtd.el`, `stfl/org-records-mcp`).
- A task that spans both is filed where the fix lands and cross-references the other tracker by ID.
- Anything deferred gets an issue: temporary pins or forks, upstream PRs to follow, workarounds to revert. A workaround in `config.org` carries the issue number in a comment beside it.

### `stfl/doom.d` is a PUBLIC repository

- **Never put sensitive information in a GitHub issue on this repo.** Deleting an issue does not reliably remove it from caches, mirrors or notification emails.
- Sensitive means, non-exhaustively: credentials, API keys, tokens, private keys; internal or LAN IP addresses, DNS topology, VPN/Tailscale configuration; employer or client names, internal project and repository identifiers; machine hostnames; absolute paths that expose a username; personal contact details.
- Scope issues to what a stranger reading the public config needs: the Emacs/Doom behaviour, the upstream package, the reproduction, the fix.
- Write paths generically (`~/.config/doom`, `<straight>/build-*/`).
- A matter that cannot be described without sensitive detail belongs in no tracker here; raise it with the user.

## Agent skills

The engineering skills read `docs/agents/*.md`; edit those files directly. Re-run `/setup-matt-pocock-skills` only to switch issue trackers or restart.

- Issue tracker: GitHub issues on `stfl/doom.d`, mechanics in `docs/agents/issue-tracker.md`; the public-repo rules above bind anything a skill publishes.
- Triage labels: the five canonical roles, each label equal to its name; see `docs/agents/triage-labels.md`.
- Domain docs: single-context, `CONTEXT.md` and `docs/adr/` at the root, created lazily; see `docs/agents/domain.md`.
