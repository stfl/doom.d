# Running and testing

Every command for tangling, syncing, linting, and running this repo's test
harnesses. The conventions an agent must follow when changing code live in
[`../CLAUDE.md`](../CLAUDE.md).

## High-Value Commands
- Re-tangle the literate config after editing `config.org`: `~/.config/emacs/bin/doom +org tangle config.org`
- Run this after config, module, or package changes: `~/.config/emacs/bin/doom sync`
- Sync and rebuild more aggressively: `~/.config/emacs/bin/doom sync --rebuild`
- Check environment and common issues: `~/.config/emacs/bin/doom doctor`
- Launch Doom using this config: `~/.config/emacs/bin/doom emacs`
- Launch an Elisp REPL: `~/.config/emacs/bin/doom emacs --repl`
- Inspect Doom CLI help: `~/.config/emacs/bin/doom help`

## Lint / Validation Commands
- Basic repository health check: `~/.config/emacs/bin/doom doctor`
- Validate that the literate config still tangles before `doom sync`: `~/.config/emacs/bin/doom +org tangle config.org`
- Byte-compile a single handwritten Elisp file: `emacs --batch -Q -L . -f batch-byte-compile my-deft-title.el`
- Byte-compile several files: `emacs --batch -Q -L . -f batch-byte-compile init.el config.el my-deft-title.el`
- Load the config noninteractively when needed: `emacs --batch -Q --load init.el`
- For risky Emacs Lisp edits, prefer byte-compilation or batch loading before finishing.

## Test Commands
- Four harnesses live under `test/`, all driven through `test/bootstrap.el`:
  - `test/org-clock-projects-smoke.el` checks the *wiring* — that the configuration hands `org-clock-projects` the resolver, settings and keys it expects. It touches no Org data.
  - `test/org-clock-projects-live.el` checks the *behaviour* against the real Org corpus: it exports the previous month for a real project into a temporary directory and compares the result against an oracle built by text search over the same `CLOCK:` lines, and against Org's own clocktable total. It writes nothing outside `$TMPDIR`.
  - `test/agile-gtd-keys.el` checks the agenda priority, TODO-state and view-range key lookups, and that `agile-gtd` defines the range commands the bindings point at. Keymap coverage for `agile-gtd` belongs here rather than in the package, because only this repo knows which keys it binds.
  - `test/agile-gtd-range-live.el` checks the *view ranges* against the real Org corpus: that every entry a range admits ranks inside that range's cutoff band, that widening never drops an entry, that work scheduled beyond today appears only at `someday`, and that the rendered agenda puts it under `Scheduled` — below the priority headings, above `Tickler`. The package's own suite proves these rules on a fixture; this proves them on the corpus they were written for. It reads Org files and renders an agenda, and writes nothing.
  - Run any of them with `emacs -q --batch -l ~/.config/doom/test/bootstrap.el -l ~/.config/doom/test/<file>`; all exit non-zero on failure.
  - None is ERT. All assert against the live configuration and the real Org corpus, which a batch ERT run cannot reach, so they report one line per check and exit on the count. The "Prefer ERT" guidance below applies to tests of handwritten helpers, not to these.
- Beyond those, verification is configuration loading, tangling, and `doom sync` success.
- Minimum validation for typical changes is: update `config.org` if needed, tangle, run `doom sync`, then open Doom if the change is user-facing.
- If you add ERT tests, keep them in a dedicated test file such as `test/<name>-test.el`.
- Run all tests in one file: `emacs --batch -Q -L . -l ert -l test/<name>-test.el --eval "(ert-run-tests-batch-and-exit t)"`
- Run a single ERT test by exact name: `emacs --batch -Q -L . -l ert -l test/<name>-test.el --eval "(ert-run-tests-batch-and-exit '^test-name$')"`
- Run tests after loading Doom if the test depends on Doom macros or modules: use the bootstrap sequence (`emacs -q --batch -l ~/.config/doom/test/bootstrap.el -l /tmp/test-file.el`).

## Single-Test Guidance
- Prefer ERT for any new automated tests.
- Name tests so they can be selected by regex without ambiguity.
- For one failing behavior, create or run one narrowly scoped ERT test instead of a broad batch.
- If a change only affects one helper function, validate that helper directly with one ERT test and byte-compilation.

