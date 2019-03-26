;;; viztab-test.el --- Tests for viztab.el

;;; Commentary:
;;
;; Tests for viztab.el
;;
;;; Code:
(require 'viztab)
(require 'cl-lib)

(defface viztab-test-face-one '((t (:foreground "Red" :slant italic))) "Viztab test face No. 1.")
(defface viztab-test-face-two '((t (:foreground "Blue" :slant bold))) "Viztab test face No. 2.")

(defun viztab-test--get-first-and-second-element (sequence)
  (list (seq-elt sequence 0) (seq-elt sequence 1)))

;;; Tests

(ert-deftest viztab-test-face-or-anonymous-face-p ()
  "Test predicate `viztab--face-or-anonymous-face-p'."
  (let ((anonymous-face-one '(:foreground "red"))
        (anonymous-face-two '(:slant italic)))
    (should (viztab--face-or-anonymous-face-p anonymous-face-one))
    (should (viztab--face-or-anonymous-face-p anonymous-face-two))
    (should (viztab--face-or-anonymous-face-p 'viztab-test-face-one))))

(ert-deftest viztab-test-table-generation-from-list ()
  (cl-letf* ((list-of-lists '(("michael" "random" "01.02.2003") ("john" "doe" "04.05.2006")))
             (list-of-vectors '(["michael" "random" "01.02.2003"] ["john" "doe" "04.05.2006"]))
             ((symbol-function 'get-first-and-second-element) (lambda (sequence) (list (seq-elt sequence 0) (seq-elt sequence 1)))))
    (should
     (equal
      (viztab-table list-of-lists nil 'get-first-and-second-element)
      '("michael random"
        "john    doe   ")))
    (should
     (equal
      (viztab-table list-of-lists (make-instance 'viztab-table-definition :column-widths '(5 3 7)))
      '("mich… ra… 01.02.…"
        "john  doe 04.05.…")))
    (should
     (equal
      (viztab-table list-of-lists '(:column-widths (5 3 7)))
      '("mich… ra… 01.02.…"
        "john  doe 04.05.…")))
    (should
     (equal
      (viztab-table list-of-vectors (make-instance 'viztab-table-definition :column-widths '(5 3 7)))
      '("mich… ra… 01.02.…"
        "john  doe 04.05.…")))))

(ert-deftest viztab-test-table-generation-empty-list ()
  (cl-letf* (((symbol-function 'get-first-and-second-element) (lambda (sequence) (list (seq-elt sequence 0) (seq-elt sequence 1)))))
    (should
     (equal
      (viztab-table nil)
      nil))
    (should
     (equal
      (viztab-table nil (make-instance 'viztab-table-definition :column-widths '(5 3 7)))
      nil))
    (should
     (equal
      (viztab-table nil (make-instance 'viztab-table-definition :column-widths '(5 3 7)) (lambda (elem) elem))
      nil))
    (should
     (equal
      (viztab-table nil (make-instance 'viztab-table-definition :column-widths '(5 3 7)) 'get-first-and-second-element)
      nil))
    (should
     (equal
      (viztab-table '([] []))
      nil))))

(ert-deftest viztab-test-table-generation-from-vector ()
  (cl-letf* ((vector-of-vectors [["michael" "random" "01.02.2003"] ["john" "doe" "04.05.2006"]])
             (vector-of-lists [("michael" "random" "01.02.2003") ("john" "doe" "04.05.2006")])
             ((symbol-function 'get-first-and-second-element) (lambda (sequence) (list (seq-elt sequence 0) (seq-elt sequence 1)))))
    (should
     (equal
      (viztab-table [])
      nil))
    (should
     (equal
      (viztab-table [] (make-instance 'viztab-table-definition :column-widths '(5 3 7)))
      nil))
    (should
     (equal
      (viztab-table [] (make-instance 'viztab-table-definition :column-widths '(5 3 7)) 'get-first-and-second-element)
      nil))
    (should
     (equal
      (viztab-table [() ()])
      nil))
    (should
     (equal
      (viztab-table vector-of-vectors nil 'get-first-and-second-element)
      '("michael random"
        "john    doe   ")))
    (should
     (equal
      (viztab-table vector-of-vectors (make-instance 'viztab-table-definition :column-widths '(5 3 7)))
      '("mich… ra… 01.02.…"
        "john  doe 04.05.…")))
    (should
     (equal
      (viztab-table vector-of-lists (make-instance 'viztab-table-definition :column-widths '(5 3 7)))
      '("mich… ra… 01.02.…"
        "john  doe 04.05.…")))))


(ert-deftest viztab-test-table-generation-with-column-face ()
  "Testing table generation with column face."
  (let* ((list-of-lists '(("michael" "random" "01.02.2003") ("john" "doe" "04.05.2006")))
         (col-widths '(7 6 10))
         (table (viztab-table list-of-lists (make-instance 'viztab-table-definition :column-face '(viztab-test-face-one viztab-test-face-two) :column-widths col-widths))))
    (let* ((first-row (car table))
           (first-col-width (car col-widths))
           (second-col-start (1+ first-col-width))
           (second-col-width (car (cdr col-widths))))
      (should (eq (get-text-property 0 'face first-row) 'viztab-test-face-one))
      (should (eq (get-text-property 0 'font-lock-face first-row) 'viztab-test-face-one))
      (should (eq (next-single-property-change 0 'face first-row) first-col-width))
      (should (eq (get-text-property second-col-start 'face first-row) 'viztab-test-face-two))
      (should (eq (next-single-property-change second-col-start 'face first-row) (+ second-col-start second-col-width))))))

(ert-deftest viztab-test-insert-table-into-buffer ()
  "Test writing of tables to buffers."
  (let* ((list-of-lists '(("michael" "random" "01.02.2003") ("john" "doe" "04.05.2006")))
         (table (viztab-table list-of-lists '(:column-face viztab-test-face-one)))
         (buffer (generate-new-buffer "*viztab test output*")))
    (unwind-protect
        (with-current-buffer buffer
          (viztab-insert-table list-of-lists '(:column-face viztab-test-face-one))
          (let* ((buffer-lines (split-string (buffer-string) "[\n]+" t)))
            (seq-mapn (lambda (line-string row-string) (should (string= line-string row-string))) buffer-lines table)))
      (kill-buffer buffer))))

;; emacs.exe --no-init-file --batch --directory=C://Users//wra//prj//viztap --load=ert --load=viztab-test --funcall=ert-run-tests-batch-and-exit

(provide 'viztab-test)
;;; viztab-test.el ends here
