# AGENTS.md

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
- `custom.el` is Emacs Custom output; avoid manual edits unless the task is explicitly about Custom-managed settings.
- Extra handwritten files include `my-deft-title.el` and JSON files in `langserver/`.

## External Agent Rules
- No `.cursorrules` file was found.
- No files were found under `.cursor/rules/`.
- No `.github/copilot-instructions.md` file was found.
- This file is therefore the main agent guidance in the repo.

## Tooling Baseline
- Emacs version on this machine is `GNU Emacs 30.2`.
- Doom CLI is available at `~/.config/emacs/bin/doom`.
- If `doom` is already on `PATH`, that is equivalent.
- There is no `Makefile`, `package.json`, CI workflow, or dedicated test directory in this repo.

## Git Sync Workflow
- This config is automatically tracked with git-sync.
- Keep git-sync stopped for the whole edit session so automated syncing does not interfere with in-progress changes.
- Do not restart git-sync if work is still uncommitted.

## Commit Workflow
- Before committing any changes to `config.org`, always run `~/.config/emacs/bin/doom sync` first — this is what actually regenerates `config.el` (via the literate module tangle hook).
- `doom +org tangle config.org` only updates explicitly-targeted blocks (`packages.el`, JSON files); it does NOT update `config.el`.
- Include `config.el` (and `packages.el` if changed) in the same commit as `config.org` so the repo stays consistent.
- Stage `config.org`, `config.el`, and `packages.el` together; never commit `config.org` alone.

## High-Value Commands
- Re-tangle the literate config after editing `config.org`: `~/.config/emacs/bin/doom +org tangle config.org`
- Run this after config, module, or package changes: `~/.config/emacs/bin/doom sync`
- Sync and rebuild more aggressively: `~/.config/emacs/bin/doom sync --rebuild`
- Check environment and common issues: `~/.config/emacs/bin/doom doctor`
- Launch Doom using this config: `~/.config/emacs/bin/doom emacs`
- Launch an Elisp REPL: `~/.config/emacs/bin/doom emacs --repl`
- Inspect Doom CLI help: `~/.config/emacs/bin/doom help`

## Local Package Development
- `agile-gtd` is developed at `~/work/agile-gtd` and `org-mcp` at `~/work/org-mcp`.
- Both are wired into Doom via `package!` `:local-repo` recipes in `config.org` (in the `** agile-gtd` and `** org-mcp` sections).
- straight.el symlinks their `.el` files into the build directory, so `doom emacs --batch` finds them automatically without any manual load-path setup.
- `:build (:not compile)` is set for both — edits to `.el` files in the local repos are live on the next Emacs session (or `eval-buffer`) without rerunning `doom sync`.
- To switch a package from local dev to the published GitHub version, swap the commented/uncommented `package!` line in `config.org`, run `doom +org tangle config.org`, then `doom sync -u`.

## Batch Eval With Doom Packages
- `doom emacs --batch` works out of the box for all Doom-managed packages including `agile-gtd` and `org-mcp`.
- Example:
  ```bash
  ~/.config/emacs/bin/doom emacs --batch -l /path/to/script.el
  ```
- For quick one-off evaluation in the running Emacs session, use `emacsclient --eval '(...)'`.
- When writing test scripts, put Elisp in a temp file and pass it via `-l` rather than fighting shell quoting with `--eval`.

## Build / Regeneration Workflow
- After changing `config.org`, run `~/.config/emacs/bin/doom sync` — this retangles `config.org` to `config.el` and syncs packages.
- `doom +org tangle config.org` only tangles blocks with explicit `:tangle` targets (e.g. `packages.el`, JSON files); it does NOT produce `config.el`.
- After editing `init.el`, run `~/.config/emacs/bin/doom sync`.
- After editing `package!` declarations, run `~/.config/emacs/bin/doom sync`.
- Never patch `config.el` by hand to avoid this workflow; regenerate it from `config.org` instead.
- Because the config explicitly disables automatic literate recompilation, do not assume tangling happens for you.
- If `config.org` changes produce updates in `config.el`, `packages.el`, or `langserver/*.json`, keep the generated files in sync.
- Use `~/.config/emacs/bin/doom sync --rebuild` only when package state, Emacs version, or stale compilation looks suspect.

## Lint / Validation Commands
- Basic repository health check: `~/.config/emacs/bin/doom doctor`
- Validate that the literate config still tangles before `doom sync`: `~/.config/emacs/bin/doom +org tangle config.org`
- Byte-compile a single handwritten Elisp file: `emacs --batch -Q -L . -f batch-byte-compile my-deft-title.el`
- Byte-compile several files: `emacs --batch -Q -L . -f batch-byte-compile init.el config.el my-deft-title.el`
- Load the config noninteractively when needed: `emacs --batch -Q --load init.el`
- For risky Emacs Lisp edits, prefer byte-compilation or batch loading before finishing.

## Debugging Startup Errors
- **`doom doctor` is the primary debug tool** for config loading errors — run it and check for lines marked `x`:
  `~/.config/emacs/bin/doom doctor 2>&1 | grep -A15 " x "`
- It reports runtime errors with backtraces, including the exact void symbol and call stack.
- Do NOT try to batch-load `config.el` directly — Doom macros (`doom!`, `after!`, etc.) are undefined without the framework.
- **`after!` load-order bugs**: if a variable used inside `(after! pkg ...)` is defined later in config.el, it works in interactive Emacs (pkg loads after config) but fails in `doom doctor` (pkg may already be loaded, so the body runs immediately). Fix by defining the variable before the `after!` block, or by adding the dependency to the `after!` condition: `(after! (pkg-a pkg-b) ...)`.
- When `doom doctor` reports `Symbol's value as variable is void`, check whether the symbol is defined later in config.el than it is used inside an `after!` body.

## Test Commands
- There is no first-class automated test suite checked into this repo today.
- Most verification is configuration loading, tangling, and `doom sync` success.
- Minimum validation for typical changes is: update `config.org` if needed, tangle, run `doom sync`, then open Doom if the change is user-facing.
- If you add ERT tests, keep them in a dedicated test file such as `test/<name>-test.el`.
- Run all tests in one file: `emacs --batch -Q -L . -l ert -l test/<name>-test.el --eval "(ert-run-tests-batch-and-exit t)"`
- Run a single ERT test by exact name: `emacs --batch -Q -L . -l ert -l test/<name>-test.el --eval "(ert-run-tests-batch-and-exit '^test-name$')"`
- Run tests after loading Doom if the test depends on Doom macros or modules: `~/.config/emacs/bin/doom emacs --repl` or a Doom-aware batch command as needed.

## Single-Test Guidance
- Prefer ERT for any new automated tests.
- Name tests so they can be selected by regex without ambiguity.
- For one failing behavior, create or run one narrowly scoped ERT test instead of a broad batch.
- If a change only affects one helper function, validate that helper directly with one ERT test and byte-compilation.

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
