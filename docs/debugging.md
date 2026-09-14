# Debugging startup errors

What to reach for when the config fails to load. Batch invocation details are in
[`batch-emacs.md`](batch-emacs.md).

- **`doom doctor` is the primary debug tool** for config loading errors — run it and check for lines marked `x`:
  `~/.config/emacs/bin/doom doctor 2>&1 | grep -A15 " x "`
- It reports runtime errors with backtraces, including the exact void symbol and call stack.
- Do NOT try to batch-load `config.el` directly — Doom macros (`doom!`, `after!`, etc.) are undefined without the framework. Use the bootstrap sequence in [`batch-emacs.md`](batch-emacs.md) instead.
- **`after!` load-order bugs**: if a variable used inside `(after! pkg ...)` is defined later in config.el, it works in interactive Emacs (pkg loads after config) but fails in `doom doctor` (pkg may already be loaded, so the body runs immediately). Fix by defining the variable before the `after!` block, or by adding the dependency to the `after!` condition: `(after! (pkg-a pkg-b) ...)`.
- When `doom doctor` reports `Symbol's value as variable is void`, check whether the symbol is defined later in config.el than it is used inside an `after!` body.

