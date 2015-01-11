;;; visual-fill-column.el --- fill-column for visual-line-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2015
;; Version: 1.0
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

;;

;;; Code:

(defgroup visual-fill-column nil "Wrap lines according to `fill-column' in `visual-line-mode'."
  :group 'wp
  :prefix "visual-fill-column-")

(defcustom visual-fill-column-width nil
  "Width of the text area.
By default, the global value of `fill-column' is used.
Alternatively, the value can be specified relative to the window
width. In this case, it must be a value between 0 and 1."
  :group 'visual-fill-column
  :type '(choice (const :tag "Use `fill-column'" nil)
                 (float :tag "Relative width:" :value 0.5)))
(make-variable-buffer-local 'visual-fill-column-width)
(put 'visual-fill-column-width 'safe-local-variable 'floatp)

(defcustom visual-fill-column-disable-fringe t
  "Disable the fringe when `visual-fill-column-mode' is active."
  :group 'visual-fill-column
  :type '(choice (const :tag "Do not disable the fringes" nil)
                 (const :tag "Disable the fringes' t")))
(make-variable-buffer-local 'visual-fill-column-disable-fringe)
(put 'visual-fill-column-disable-fringe 'safe-local-variable 'symbolp)

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
  (set-window-margins (selected-window) 0 0)
  (when visual-fill-column-disable-fringe
    (set-window-fringes (selected-window) nil nil)))

(defun visual-fill-column--adjust-window ()
  "Adjust the window margins and fringes."
  (visual-fill-column--set-fringes)
  (visual-fill-column--set-margins))

(defun visual-fill-column--set-fringes ()
  "Set the fringes for the current window."
  (when visual-fill-column-disable-fringe
    ;; the left fringe is only disabled if the text is centered
    (let ((left (if visual-fill-column-center-text 0 nil))
          (right 0))
      ;; swap left & right fringes in explicitly R2L buffers
      (when (eq bidi-paragraph-direction 'right-to-left)
        (setq right left)
        (setq left 0))
      (set-window-fringes (selected-window) left right))))

(defun visual-fill-column--set-margins ()
  "Set window margins for the current window."
  ;; calculate left & right margins
  (let* ((window (selected-window))
         (current-width (window-total-width window))
         (margins (cond
                   ((and (floatp visual-fill-column-width)
                         (< 0 visual-fill-column-width 1))
                    (- current-width (truncate (* current-width visual-fill-column-width))))
                   (t (if (< (- current-width fill-column) 0) ; margins must be >= 0
                          0
                        (- current-width fill-column)))))
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
