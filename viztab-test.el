;;; viztab-test.el --- Tests for viztab.el

;;; Commentary:

;;; Code:
(load-file (expand-file-name "viztab.el" default-directory))

(defvar viztab--test-list-data '(("michael" "random" "01.02.2003") ("john" "doe" "04.05.2006"))
  "Test data for viztab instances.")

(defface viztab-test-face '((t (:foreground "Red" :slant italic))) "Viztab test face.")

(defun viztab-test--get-first-and-second-element (sequence)
  (list (seq-elt sequence 0) (seq-elt sequence 1)))

(defun viztab-test--create-list-test-instance ()
  "Return a viztab test instance using a list data structure."
  (make-instance 'viztab
                 :data viztab--test-list-data
                 :splitter 'viztab-test--get-first-and-second-element))


;;; Tests

(ert-deftest viztab-test-face-or-anonymous-face-p ()
  "Test predicate `viztab--face-or-anonymous-face-p'."
  (let ((anonymous-face-one '(:foreground "red"))
        (anonymous-face-two '(:slant italic)))
    (should (viztab--face-or-anonymous-face-p anonymous-face-one))
    (should (viztab--face-or-anonymous-face-p anonymous-face-two))
    (should (viztab--face-or-anonymous-face-p 'viztab-test-face))))

(ert-deftest viztab-test-table-generation-list-type ()
  "Testing viztab functionality for tables."
  (let ((table (viztab-test--create-list-test-instance)))
    (viztab-update-visual-rows table)
    (should (equal (oref table visual-rows) '("michael random"
                                              "john    doe   ")))

    (viztab-update-visual-rows table '(5 3))
    (should (equal (oref table visual-rows) '("mich… ra…"
                                              "john  doe")))
    (should (equal (oref table column-widths) '(5 3)))))

(ert-deftest viztab-test-table-generation-with-column-face ()
  "Testing table generation with column face."
  (let ((table (viztab-test--create-list-test-instance)))
    (oset table :column-face 'viztab-test-face)
    (viztab-update-visual-rows table)
    (let ((first-row (car (oref table visual-rows)))
          (first-col-width (car (oref table column-widths))))
      (should (eq (get-text-property 0 'face first-row) 'viztab-test-face))
      (should (eq (get-text-property 0 'font-lock-face first-row) 'viztab-test-face))
      (should (eq (next-single-property-change 0 'face first-row) first-col-width)))))

(ert-deftest viztab-test-writing-table-to-buffer ()
  "Test writing of tables to buffers."
  (let ((table (viztab-test--create-list-test-instance))
        (buffer (generate-new-buffer "*viztab test output*")))
    (unwind-protect
        (progn
          (oset table :column-face 'viztab-test-face)
          (viztab-update-visual-rows table)
          (viztab--write-table-to-buffer table buffer)
          (with-current-buffer buffer
            (let* ((buffer-lines (split-string (buffer-string) "[\n]+" t)))
              (seq-mapn (lambda (line-string row-string) (should (string= line-string row-string))) buffer-lines (oref table visual-rows)))))
      (kill-buffer buffer))))

(provide 'viztab-test)
;;; viztab-test.el ends here
