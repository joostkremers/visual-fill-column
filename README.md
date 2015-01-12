# `visual-fill-column-mode` #

`visual-fill-column-mode` is a small minor mode that mimics the effect of `fill-column` in `visual-line-mode`. Instead of wrapping lines at the window edge, which is the standard behaviour of `visual-line-mode`, it wraps lines at `fill-column`. If `fill-column` is too large for the window, the text is wrapped at the window edge.


## Installation ##

To install `visual-fill-column-mode`, simply put `visual-fill-column-mode.el` in your load path and add the following lines to your `init.el`:

```lisp
(require 'visual-fill-column)
(global-visual-fill-column-mode)
```

The command `global-visual-fill-column-mode` turns on `visual-fill-column-mode` in all buffers that use `visual-line-mode`.


## Usage ##

To use `visual-fill-column-mode` with `visual-line-mode`, use the command `global-visual-fill-column-mode`. Alternatively, you can use the command `visual-fill-column-mode` in mode hooks. Note that it is perfectly possible to use `visual-fill-column-mode` independently from `visual-line-mode`.

`visual-fill-column-mode` works by widening the right window margin. This reduces the area that is available for text display, creating the appearance that the text is wrapped at `fill-column`. In buffers that are explicitly right-to-left (i.e., those where `bidi-paragraph-direction` is set to `right-to-left`), the left margin is expanded, so that the text appears at the window’s right margin.

Widening the margin has the effect that the fringe is pushed inward. Since the fringe is visible, this has a somewhat disturbing, because it looks like there’s another window to the left. For this reason, the left fringe is disabled by default.


## Options ##

`visual-fill-column-width`: column at which to wrap lines. If set to `nil` (the default), use the value of `fill-column` instead. Can also be set to a fraction between `0` and `1`, in which case the text width is relative to the window width.

`visual-fill-column-center-text`: if set to `t`, centre the text area in the window. By default, the text is displayed at the window’s (left) edge, mimicking the effect of `fill-column`.

`visual-fill-column-disable-fringe`: if set to `t`, the left fringe is disabled. Note that if `visual-fill-column-center-text` is set to `t`, both the left and right fringes are disabled.
