# CLAUDE.md

`AGENTS.md` is a symlink to this file. Edit this one.

This file holds what an agent changing the code must keep true: the repo's
invariants, conventions, and obligations. Commands and procedures a human also
needs live in `docs/`:

| Page | Answers |
|---|---|
| [`docs/running-and-testing.md`](docs/running-and-testing.md) | How do I tangle, sync, lint, and run the test harnesses? |
| [`docs/batch-emacs.md`](docs/batch-emacs.md) | How do I query a live Emacs, or load the config in batch? |
| [`docs/debugging.md`](docs/debugging.md) | The config won't load — what do I run? |
| [`docs/local-packages.md`](docs/local-packages.md) | How are `agile-gtd` and `org-mcp` wired in from local checkouts? |

A new fact earns a place in this file only by constraining how code is changed.
A command, a procedure, or a troubleshooting recipe goes to `docs/` and is
linked from the table above.

## Purpose
- This repository is a personal Doom Emacs configuration.
- The main languages are Emacs Lisp and Org.
- Agents should optimize for small, low-risk changes that fit existing Doom idioms.
- For configuration work, edit `config.org`, not `config.el`, and finish by running `doom sync`.

## Repo Snapshot
- Primary source of truth is `config.org`, and agents should default to editing it for configuration changes.
- `init.el` is hand-maintained and declares enabled Doom modules.
- `config.el` is a tangled/generated output from the literate config and should never be edited directly.
- `packages.el` is also driven by tangled `package!` blocks from `config.org`.
- `custom.el` is Emacs Custom output; avoid manual edits to this file!
- Extra handwritten files include `my-deft-title.el` and JSON files in `langserver/`.

## Tooling Baseline
- Emacs version on this machine is `GNU Emacs 31.1`.
- Doom CLI is available at `~/.config/emacs/bin/doom`.
- If `doom` is already on `PATH`, that is equivalent.
- There is no `Makefile`, `package.json`, or CI workflow in this repo.
- Tests live in `test/` and are run by hand; see [`docs/running-and-testing.md`](docs/running-and-testing.md).

## Commit Workflow
- Before committing any changes to `config.org`, always run `~/.config/emacs/bin/doom sync` first — this is what actually regenerates `config.el` (via the literate module tangle hook).
- `doom +org tangle config.org` only updates explicitly-targeted blocks (`packages.el`, JSON files); it does NOT update `config.el`.
- Include `config.el` (and `packages.el` if changed) in the same commit as `config.org` so the repo stays consistent.
- Stage `config.org`, `config.el`, and `packages.el` together; never commit `config.org` alone.

## Issue Tracking
- Issues for this repo live as GitHub issues on `stfl/doom.d`: `gh issue create --repo stfl/doom.d ...`.
- Do not track this repo's work in the `dotfiles` beads database, or in any other repo's tracker.
- The split is by subject, not by which directory you happen to be in: Doom/Emacs/Org/package work goes to `stfl/doom.d` GitHub issues; NixOS, Home Manager, host, and system-level work stays in the `dotfiles` beads database (`bd create` from `~/.config/dotfiles`).
- When a task spans both, file it where the fix lands and cross-reference the other tracker by ID.
- Use an issue for anything deferred: temporary pins or forks, upstream PRs to follow, and workarounds to revert.
- When a workaround lands in `config.org`, reference the issue number in a comment next to it so the two stay linked.

### `stfl/doom.d` is a PUBLIC repository
- **Never put sensitive information in a GitHub issue on this repo.** Once published it is public permanently, and deleting the issue does not reliably remove it from caches, mirrors, or notification emails.
- Sensitive means, non-exhaustively: credentials, API keys, tokens, PATs, private keys; internal or LAN IP addresses, DNS topology, and VPN/Tailscale configuration; employer or client names, internal project and repository identifiers; machine hostnames; absolute paths that expose a username; personal contact details.
- Keep issues scoped to what a stranger reading the public config needs: the Emacs/Doom behaviour, the upstream package, the reproduction, the fix.
- Refer to paths generically (`~/.config/doom`, `<straight>/build-*/`) instead of pasting absolute local paths.
- If something genuinely cannot be described without sensitive detail, it does not belong in this tracker — raise it with the user instead of sanitising it into uselessness.

## Agent skills

Configuration the engineering skills read. Edit `docs/agents/*.md` directly to change any of it; re-run `/setup-matt-pocock-skills` only to switch issue trackers or restart from scratch.

### Issue tracker

GitHub issues on `stfl/doom.d`. See `## Issue Tracking` above for the public-repo sensitivity rules and the doom/dotfiles split — those constrain anything a skill publishes. Mechanics in `docs/agents/issue-tracker.md`.

### Triage labels

The five canonical roles, each label string equal to its name. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` and `docs/adr/` at the repo root, both created lazily. See `docs/agents/domain.md`.

## Build / Regeneration Workflow
- After changing `config.org`, run `~/.config/emacs/bin/doom sync` — this retangles `config.org` to `config.el` and syncs packages.
- `doom +org tangle config.org` only tangles blocks with explicit `:tangle` targets (e.g. `packages.el`, JSON files); it does NOT produce `config.el`.
- After editing `init.el`, run `~/.config/emacs/bin/doom sync`.
- After editing `package!` declarations, run `~/.config/emacs/bin/doom sync`.
- Never patch `config.el` by hand to avoid this workflow; regenerate it from `config.org` instead.
- Because the config explicitly disables automatic literate recompilation, do not assume tangling happens for you.
- If `config.org` changes produce updates in `config.el`, `packages.el`, or `langserver/*.json`, keep the generated files in sync.
- Use `~/.config/emacs/bin/doom sync --rebuild` only when package state, Emacs version, or stale compilation looks suspect.

## File Editing Priorities
- Always edit `config.org` for literate configuration changes.
- Never edit `config.el` directly; regenerate it from `config.org` instead.
- If a setting already lives in `config.org`, update it there even if the generated `config.el` looks easier to patch.
- Edit `init.el` directly for Doom module selection.
- Edit `my-deft-title.el` directly; it is a normal handwritten library.
- Avoid hand-editing `custom.el` unless required by the task.
- Treat `langserver/*.json` as generated if the corresponding Org source block exists in `config.org`.

## Code Style
- Follow existing Doom Emacs conventions instead of introducing generic Emacs Lisp patterns.
- Use lexical binding where the file already enables it; do not remove it.
- Prefer standard Emacs Lisp indentation and alignment.
- Use spaces for indentation in source; do not introduce hard tabs in code.
- Keep comments sparse and useful; most current code is minimally commented.
- Preserve the repo's direct, pragmatic style over abstract framework-building.

## Imports and Dependencies
- When inspecting dependency implementations, always prefer the local straight checkouts under `~/.config/emacs/.local/straight/repos/*` as the source of truth.
- Prefer plain `with-eval-after-load`, `use-package`, and `setopt` in private config, following recent Doom upstream guidance.
- Use Doom-specific macros such as `map!`, `add-hook!`, and `defadvice!` where they remain the clearest fit.
- Put package-specific configuration inside `with-eval-after-load` or `use-package` blocks unless a Doom-only form is required.
- Use `require` only when eager loading is actually needed.
- Add new packages in tangled `package!` blocks, not ad hoc runtime installs.
- Disabled packages are expressed in `packages.el` as `:disable t`.

## Formatting Conventions
- Prefer `setopt` for customizable variables and `setq` elsewhere.
- Existing code still contains legacy `setq!`; do not mass-convert it unless the task calls for it.
- Keep related settings grouped inside one form when it improves readability.
- Multi-line forms usually place one binding per line.
- Keybinding blocks are usually grouped with one `map!` per context.
- Preserve quote style already used in nearby code.
- Match surrounding whitespace instead of reformatting unrelated code.

## Types and Data Shapes
- Emacs Lisp in this repo is dynamically typed.
- Be explicit about expected shapes in docstrings when a function takes complex plist/alist data.
- Existing code heavily uses lists, plists, alists, markers, and Org elements.
- When extending data structures, preserve current key names and value formats.

## Naming Conventions
- Prefix repo-specific functions and variables with `stfl/`.
- Preserve existing third-party or borrowed prefixes such as `my-deft/` and `ibizaman/`.
- Use kebab-case for function and variable names.
- Use `--` for private helpers only when following an existing local pattern.
- Interactive commands should usually have clear verb-based names.

## Error Handling
- Use `user-error` for bad interactive input when the user can correct it.
- Use `error` for invariant violations or truly unexpected states.
- Use `ignore-errors` or `ignore-error` sparingly and only around best-effort behavior.
- Preserve existing interactive safety checks instead of silently swallowing failures.
- If a function mutates user data or Org state, prefer explicit failure over partial silent success.

## Doom / Org Patterns To Preserve
- Many settings are wrapped in `after! org`, `after! org-roam`, or other package-specific blocks.
- Keybindings are organized with `:leader`, `:localleader`, `:prefix`, and mode maps.
- Org code relies heavily on agenda queries, custom commands, capture templates, and property drawers.
- Org-specific helpers often assume agenda markers, headline context, or inherited properties.
- When changing Org behavior, watch for interactions with `org-agenda`, `org-roam`, `org-ql`, and `org-super-agenda`.

## Change Strategy For Agents
- Read the surrounding block before editing; many sections are tightly coupled.
- Keep changes local and incremental.
- Do not perform broad stylistic rewrites.
- Default to `config.org` for config changes and regenerate derived files instead of patching generated outputs.
- Treat direct edits to `config.el` as incorrect unless the task is explicitly about generated output debugging.
- Do not replace Doom macros with vanilla alternatives unless there is a strong repo-specific reason.
- This repo still contains legacy `after!`, `use-package!`, and `setq!` usage; prefer newer forms in touched code when low-risk, but do not perform broad mechanical rewrites unless requested.
- When changing literate config, update the Org source first and then regenerate outputs.
- Mention any generated-file updates in your final note.

## Verification Checklist
- After a change, run `doom sync`, and include the updated `config.el` (and `packages.el` if changed) in the commit.
- If you touched generated files because tangling updated them, verify they came from `config.org` and were not edited by hand.
- If you changed handwritten Elisp helpers, byte-compile or batch-load the touched file.
- If behavior is interactive, open Doom and smoke-test the exact command or keybinding you changed.
- If you added tests, include the exact single-test command in your handoff.
