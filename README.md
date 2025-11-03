## Emacs VTerm

![Emacs VTerm Icon](icons/emacs-vterm.png)

[![Emacs VTerm 30](https://github.com/binbinsh/homebrew-emacs-vterm/actions/workflows/emacs-vterm.yml/badge.svg)](https://github.com/binbinsh/homebrew-emacs-vterm/actions/workflows/emacs-vterm.yml)

Emacs VTerm as default terminal for macOS. This tap provides a customized Emacs 30 build that launches VTerm by default and ships with a modern macOS app bundle named `Emacs VTerm.app`.

This project is based on and adapted from `homebrew-emacs-plus` — many thanks to the original authors and contributors. License and most build options follow upstream. See the upstream repository for in-depth option descriptions and discussion.

- Upstream: [`d12frosted/homebrew-emacs-plus`](https://github.com/d12frosted/homebrew-emacs-plus)
- License: MIT (same as upstream)

### Install

```bash
brew tap binbinsh/emacs-vterm
brew install emacs-vterm
```

After install, the application bundle is placed at:

```
$(brew --prefix)/opt/emacs-vterm/Emacs VTerm.app
```

### Behavior

- On GUI launches with no arguments, Emacs opens VTerm by default.
- If `vterm` is not installed yet, install it via `M-x package-install RET vterm RET`.
- Normal behavior is preserved (e.g., `emacs-vterm file.txt`, `--daemon`, `--batch`).

### Icon

- Default icon is `icons/emacs-vterm.png` (converted to `.icns` during build).

### CLI

One shim is provided that launches the app’s bundled executable:

```bash
emacs-vterm
```

### Notes

- PATH injection is handled via a small wrapper to keep your shell PATH inside the GUI app (same approach as upstream; set `EMACS_PLUS_NO_PATH_INJECTION=1` to disable).
- Build is based on Emacs 30 (stable) only. HEAD can be enabled with standard flags.

### Credits

This work builds directly on the excellent `homebrew-emacs-plus` project and reuses its formula structure, patches, and options where appropriate. See upstream for detailed documentation and discussion of build flags and patches.


