;;; visual-fill-column.el --- fill-column for visual-line-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2015
;; Version: 1.3
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; visual-fill-coulmn is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; visual-fill-coulmn is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `visual-fill-column-mode' is a small Emacs minor mode that mimics the
;; effect of `fill-column' in `visual-line-mode'. Instead of wrapping lines
;; at the window edge, which is the standard behaviour of
;; `visual-line-mode', it wraps lines at `fill-column'. If `fill-column' is
;; too large for the window, the text is wrapped at the window edge.

;;; Code:

(defgroup visual-fill-column nil "Wrap lines according to `fill-column' in `visual-line-mode'."
  :group 'wp
  :prefix "visual-fill-column-")

(defcustom visual-fill-column-width nil
  "Width of the text area.
By default, the global value of `fill-column' is used, but if
this option is set to a value, it is used instead."
  :group 'visual-fill-column
  :type '(choice (const :tag "Use `fill-column'" :value nil)
                 (integer :tag "Specify width" :value 70)))
(make-variable-buffer-local 'visual-fill-column-width)
(put 'visual-fill-column-width 'safe-local-variable 'numberp)

(defcustom visual-fill-column-fringes-outside-margins t
  "Put the fringes outside the margins."
  :group 'visual-fill-column
  :type '(choice (const :tag "Put fringes outside the margins" t)
                 (const :tag "Keep the fringes inside the margins" nil)))
(make-variable-buffer-local 'visual-fill-column-fringes-outside-margins)
(put 'visual-fill-column-fringes-outside-margins 'safe-local-variable 'symbolp)

(defcustom visual-fill-column-center-text nil
  "If set, center the text area in the window."
  :group 'visual-fill-column
  :type '(choice (const :tag "Display text area at window margin" nil)
                 (const :tag "Center text area" t)))
(make-variable-buffer-local 'visual-fill-column-center-text)
(put 'visual-fill-column-center-text 'safe-local-variable 'symbolp)

;;;###autoload
(define-minor-mode visual-fill-column-mode
  "Wrap lines according to `fill-column' in `visual-line-mode'."
  :init-value nil :lighter nil :global nil
  (if visual-fill-column-mode
      (visual-fill-column-mode--enable)
    (visual-fill-column-mode--disable)))

;;;###autoload
(define-globalized-minor-mode global-visual-fill-column-mode visual-fill-column-mode turn-on-visual-fill-column-mode
  :require 'visual-fill-column-mode
  :group 'visual-fill-column)

(advice-add 'split-window
    :around #'visual-fill-column--disable-on-split-window)

(defun visual-fill-column--disable-on-split-window (fn window &rest args)
  "Undo the effects of `visual-fill-column-mode' for splitting window."
  (if (and (or (not window) (window-live-p window))
           (buffer-local-value 'visual-fill-column-mode
                               (window-buffer (or window (selected-window)))))
    (let ((inhibit-redisplay t))
      (set-window-fringes (or window (selected-window)) nil)
      (set-window-margins (or window (selected-window)) 0 0)
      (unwind-protect (apply fn window args)
        (save-selected-window
          (when window (select-window window 'norecord))
          (visual-fill-column--adjust-window))))
    (apply fn window args)))

(defun turn-on-visual-fill-column-mode ()
  "Turn on `visual-fill-column-mode'.
Note that `visual-fill-column-mode' is only turned on in buffers
in which `visual-line-mode' is active as well."
  (when visual-line-mode
    (visual-fill-column-mode 1)))

(defun visual-fill-column-mode--enable ()
  "Set up `visual-fill-column-mode' for the current buffer."
  (add-hook 'window-configuration-change-hook #'visual-fill-column--adjust-window nil t)
  (visual-fill-column--adjust-window))

(defun visual-fill-column-mode--disable ()
  "Disable `visual-fill-column-mode' for the current buffer."
  (remove-hook 'window-configuration-change-hook #'visual-fill-column--adjust-window t)
  (set-window-fringes (selected-window) nil)
  (set-window-margins (selected-window) 0 0))

(defun visual-fill-column--adjust-window ()
  "Adjust the window margins and fringes."
  (set-window-fringes (selected-window) nil nil visual-fill-column-fringes-outside-margins)
  (visual-fill-column--set-margins))

(defun visual-fill-column--window-body-width (&optional window)
  "Return the body width of WINDOW.
The body width here refers to the width of the text area plus the
margins, but excluding the fringes, scroll bar and right
divider. WINDOW defaults to the selected window."
  (or window (setq window (selected-window)))
  (let ((margins (window-margins window)))
    (+ (window-width window)
       (or (car margins) 0)
       (or (cdr margins) 0))))

(defun visual-fill-column--set-margins ()
  "Set window margins for the current window."
  ;; calculate left & right margins
  (let* ((window (selected-window))
         (total-width (visual-fill-column--window-body-width window))
         (width (or visual-fill-column-width
                    fill-column))
         (margins (if (< (- total-width width) 0) ; margins must be >= 0
                      0
                    (- total-width width)))
         (left (if visual-fill-column-center-text
                   (/ margins 2)
                 0))
         (right (if visual-fill-column-center-text
                    (/ margins 2)
                  margins)))
    ;; put an explicitly R2L buffer on the right side of the window
    (when (and (eq bidi-paragraph-direction 'right-to-left)
               (= left 0))
      (setq left right)
      (setq right 0))
    ;; check values and set the margins
    (set-window-margins window left right)))

(provide 'visual-fill-column)

;;; visual-fill-column.el ends here
