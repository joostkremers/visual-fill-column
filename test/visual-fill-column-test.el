;;; visual-fill-column-test.el --- Tests for visual-fill-column

;;; Commentary:
;; Tests for the visual-fill-column package.

;;; Code:

(require 'visual-fill-column)

(ert-deftest visual-fill-column--add-extra-width ()
  (should (equal (visual-fill-column--add-extra-width 0  80 '(4 . 4)) '(0 . 76)))
  (should (equal (visual-fill-column--add-extra-width 10 80 '(4 . 0)) '(6 . 80)))
  (should (equal (visual-fill-column--add-extra-width 0  80 '(-4 . 0)) '(4 . 80))))

;;; visual-fill-column-test.el ends here
