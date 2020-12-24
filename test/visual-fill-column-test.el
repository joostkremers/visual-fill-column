;;; visual-fill-column-test.el --- Tests for visual-fill-column

;;; Commentary:
;; Tests for the visual-fill-column package.

;;; Code:

(require 'visual-fill-column)

(ert-deftest visual-fill-column--calculate-margin-shift ()
  (should (equal (visual-fill-column--calculate-margin-shift 0  80 4)  '(4 . 76)))
  (should (equal (visual-fill-column--calculate-margin-shift 10 80 -4) '(6 . 84)))
  (should (equal (visual-fill-column--calculate-margin-shift 0  80 -4) '(0 . 80)))
  (should (equal (visual-fill-column--calculate-margin-shift 80 0  4)  '(80 . 0))))

;;; visual-fill-column-test.el ends here
