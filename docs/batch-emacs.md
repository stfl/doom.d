# Driving Emacs from the shell

How to query a running Emacs and how to load the full Doom config in a batch
sandbox, plus the batch invocations that look right but silently skip your
config. Startup errors are covered in [`debugging.md`](debugging.md).

## Inspecting the Running Emacs Session (read-only)

Use `emacsclient` to query the live Emacs state without touching the session:

```bash
emacsclient -e '(length org-agenda-files)'
emacsclient -e 'org-mcp-allowed-files'
emacsclient -e '(featurep (quote agile-gtd))'
```

**Keep emacsclient calls read-only.** Do not use it to call `setq`, `load`, or any mutating form — that would silently change the running session. Use it only to read variable values, check feature flags, or inspect state.

When writing multi-expression queries, put the Elisp in a temp file and load it:
```bash
emacsclient -e "(load-file \"/tmp/query.el\")"
```

## Loading the Full Doom Config in a Batch Sandbox

To run tests or verify config changes without a running Emacs, use the checked-in bootstrap script at `test/bootstrap.el`. It drives the Doom init chain manually, bypassing the `noninteractive` check that normally skips user config in batch mode.

```bash
emacs -q --batch -l ~/.config/doom/test/bootstrap.el -l /tmp/your-test.el 2>/dev/null
```

Write temporary test scripts to `/tmp/` — do not leave scratch `.el` files in the repo. After bootstrap, `(require 'org)` fires all `eval-after-load 'org` hooks (agile-gtd, org-mcp, etc.) exactly as in interactive Emacs. Use `(message ...)` for output — it goes to stderr, so redirect with `2>&1` or `2>/tmp/out.txt` to capture it.

**Critical constraint — do NOT set `DOOMPROFILE` in the environment.** When `DOOMPROFILE` is set, `doom-data-dir` switches to `~/.local/share/doom/` and `doom-profile-init-file` resolves to a non-existent path. Leave `DOOMPROFILE` unset so `doom-profile` stays `nil` and `doom-data-dir` stays at `.local/etc/` where the generated init lives.

## Batch Options That Do NOT Load User Config

These commands are commonly tried but do not load `config.el`:

| Command | Why it fails |
|---|---|
| `doom emacs --batch -l script.el` | Uses `-q` internally; no early-init, no packages on load-path, no user config |
| `emacs --init-directory ~/.config/emacs --batch -l script.el` | `doom-initialize (not noninteractive)` takes the CLI path, skipping user config |
| `emacs --init-directory ... --batch --eval '(require (quote org))'` | `--eval` with shell-special chars breaks argument parsing; use `-l` instead |
| `doom emacs --repl` | Minimal Doom CLI Emacs; `doom-user-dir` is empty, no user config |

