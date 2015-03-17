# Visual Fill Column #

`visual-fill-column-mode` is a small Emacs minor mode that mimics the effect of `fill-column` in `visual-line-mode`. Instead of wrapping lines at the window edge, which is the standard behaviour of `visual-line-mode`, it wraps lines at `fill-column`. If `fill-column` is too large for the window, the text is wrapped at the window edge.


## Installation ##

Install `visual-fill-column-mode` via [Melpa](http://melpa.org), or put `visual-fill-column-mode.el` in your load path, (optionally) byte-compile it, and add `(require ’visual-fill-column)` to your `init.el`.


## Usage ##

`visual-fill-column-mode` is primarily intended to be used alongside `visual-line-mode`. To activate `visual-fill-column-mode` automatically in every buffer that uses `visual-line-mode`, customise the option `global-visual-fill-column-mode` or add the command `(global-visual-fill-column-mode)` to your init file.

However, `visual-fill-column-mode` is not tied to `visual-line-mode`: it is perfectly possible to use it on its own, in buffers that use some other word-wrapping method (e.g., `auto-fill-mode`), or in buffers that do not wrap at all. You can activate it interactively with `visual-fill-column-mode` or you can add the command `visual-fill-column-mode` in mode hooks.

`visual-fill-column-mode` works by widening the right window margin. This reduces the area that is available for text display, creating the appearance that the text is wrapped at `fill-column`. The amount by which the right margin is widened depends on the window width and is automatically adjusted when the window’s width changes (e.g., when the window is split in two side-by-side windows).

In buffers that are explicitly right-to-left (i.e., those where `bidi-paragraph-direction` is set to `right-to-left`), the left margin is expanded, so that the text appears at the window’s right side.

Widening the margin causes the fringe to be pushed inward. For this reason, the fringes are placed outside the margins by setting the variable `fringes-outside-margins` to `t`.


## Options ##

`visual-fill-column-width`: column at which to wrap lines. If set to `nil` (the default), use the value of `fill-column` instead.

`visual-fill-column-center-text`: if set to `t`, centre the text area in the window. By default, the text is displayed at the window’s (left) edge, mimicking the effect of `fill-column`.

`visual-fill-column-fringes-outside-margins`: if set to `t`, put the fringes outside the margins.

All three options are buffer-local, so the values you set through Customize are default values. They can also be set in mode hooks or directory or file local variables in order to customise particular files or file types.
