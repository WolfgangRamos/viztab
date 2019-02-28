;;; viztab.el --- Visualize (Data) Structures as tables

;; Copyright (C) 2019

;; Author: Wolfgang Ramos <wolfgang.ramos@gmail.com>
;; Maintainer: Wolfgang Ramos <wolfgang.ramos@gmail.com>
;; URL: https://github.com/WolfgangRamos/viztab
;; Package-Version: 20190228
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (cl-generic "0.3"))

;; This file is part of GNU Emacs.

;;; Commentary:

;; Viztab.el allows for easy formatting two level nested list or
;; vector structures like `([A B C] [D E F] [G H I])' as tables
;; like:

;;                          | A | B | C |
;;                          | D | E | F |
;;                          | G | H | I |

;; The formatting result is mainly intended to be used in code, e.g.
;; as display values in completion frameworks like `helm' or `ivy'
;; that allow for completing arbitrary objects from a vertical list of
;; objects (or string representations of those objects). Therefore the
;; formatting result is provided as list of row strings. Together with
;; one of the mentioned completion framework the list of row strings
;; can be used to display multiple fields of those objects aligned in
;; columns, supporting the process of selection by:

;; (1) displaying more than one field

;; (2) displaying the fields in a way that users can easily read,
;;     destinguish and filter.

;; Additionally the generated tables can also be visualized within a
;; dedicated buffer or inserted as string into any buffer.

;; Supported data structures are:

;; (1) a list of Lisp objects, e.g. a list of lists like `((A B C) (D
;;     E F) (G H I))' or a list of `eieio' objects.

;; (2) a vector of Lisp objects, e.g. a vector of vectors like `[[A B C]
;;     [D E F] [G H I]]' or a vector of `eieio' objects.

;; To format one of these structures as table you

;; (1) Create an instance of type `viztab',

;; (2) Set its `:data' field to the nested list or vectors structure
;;     that contains the objects you want to format as table.

;; (3) Optionally set the `:splitter' field of your object to a
;;     function that extracts the fields you want to display from your
;;     objects.

;; (4) Call `viztab-update-visual-rows' on your instance.

;; (5) Extract the formatted row strings from the field `:visual-rows'
;;     of your instance and use them in your code or display your
;;     table by calling `viztab-show-table' or
;;     `viztab-show-table-other-window' on your instance.

;; If you do not want to create an instance you can also use
;; `viztab-format-as-table' to format a nested vector or list
;; structure as table.

;; An example result of formatting one of these data structures as
;;
;; The formatting result can be further adjusted by:

;; - Customizing how elements of your list or vector structure
;;   (i.e. row elements) are transformed into lists of strings (i.e.
;;   cells). E.g. you could use the first and third element of
;;   each nested list and omit the second.

;; - Customizing the column seperator, as well as the row start and
;;   end strings and their faces.

;; - Setting maximum columns widths (cell strings are trimmed
;;   accordingly).

;; - Customizing the trim indicator that is appended to a trimmed
;;   string to inform the user that the cell string was trimmed.

;; - Add faces to columns.

;; A demonstration of all features can be found in `viztab-demo.el'.

;;; Change Log:

;; - Version 1.0
;;   - created class viztab
;;   - created functions for creating cell and row strings
;;   - created test cases

;;; Code:

(require 'eieio)
(require 'cl-generic)
(require 'seq)

(defclass viztab ()
  ((data
    :initarg :data
    :type (or list vector)
    :documentation
    "The data from which the table is created. The data must be
    either (1) a list of lists, (2) a list of vectors, (3) a
    vector of lists, or (4) a vector of vectors.")
   (column-seperator
    :initarg :column-seperator
    :type (or string list)
    :initform " "
    :documentation
    "The string used to seperator columns or a list of strings
    used to seperate columns. In the latter case the list
    elements are repeated if the number of columns exceeds the
    number of column seperators. Vice versa, if the number of
    column seperators exceeds the number of columns the
    superfluous column seperators at the lists end are ignored.
    Text properties within this string / these strings are
    respected.")
   (column-start
    :initarg :column-start
    :type string
    :initform ""
    :documentation
    "The string used as the left outer border of the left most
    cell of each row. Text properties within this string are
    respected.")
   (column-end
    :initarg :column-end
    :type string
    :initform ""
    :documentation
    "The string used as the right outer border of the right most
    cell of each row. Text properties within this string are
    respected.")
   (trim-indicator
    :initarg :trim-indicator
    :type string
    :initform "…"
    :documentation
    "The string appended to trimmed cell strings. Only cell
    strings that exceed their columns maximum widths are
    trimmed (see `:column-widths'). Trimming is performed so that
    the combination of cell and trim indicator does not exceed
    the columns maximum width. Text properties within this string
    are respected.")
   (column-face
    :initarg
    :column-face (or face list)
    :documentation
    "The named or anonymous face that should be applied to each
    column or a list of named or anonymous faces applied to the
    columns. In the latter case the list elements are repeated if
    the number of columns exceeds the number of given faces. Vice
    versa, if the number of given faces exceeds the number of
    columns the superfluous faces at the lists end are ignored.")
   (column-widths
    :initform nil
    :type list
    :documentation
    "The column widths in numer of characters.")
   (splitter
    :initarg :splitter
    :type function
    :initform 'identity
    :documentation
    "A function to split a row object (i.e. an element of
    `:data') into individual cells. This function must return a
    sequence. This function is applied to each element or
    row-objects to generate the columns.")
   (visual-rows
    :type list
    :documentation
    "A list of rows. Each row is a list of cells. Each cell is a
    string trimmed to its column width (see
    `trim-indicator').")))

(cl-defgeneric viztab-split-rows ()
  "Split row objects into columns.")

(cl-defmethod viztab-split-rows ((table viztab))
  "Split row objects into columns."
  (mapcar (oref table splitter) (oref table data)))

(defun viztab--compute-column-widths (columns)
  "Compute column widths of data in COLUMNS."
  (let ((column-widths (and (seqp columns) columns (make-list (seq-length (seq-elt columns 0)) 0))))
    (seq-reduce #'(lambda (widths column) (seq-mapn 'max widths (seq-map 'length column))) columns column-widths)))

;;(viztab--compute-column-widths (viztab-split-rows viztab-test-instance))

(defun viztab--make-cell-string (str length trim-indicator &optional cell-face)
  "Create a cell string by trimming STR to LENGTH.
If the length of STR exceeds LENGTH then STR is trimmed so that
the result of appending TRIM-INDICATOR to the trimmed STR results
in a string of length LENGTH.

If CELL-FACE is a named or anonymous face it will be used as
`face' and `font-lock-face' text property of the resulting
string."
  (let* ((str-length (length str))
         (trim-indicator-length (length trim-indicator))
         (end (- length trim-indicator-length))
         (length-diff (- length str-length))
         (cell-string (if (< length-diff 0)
                          (concat (substring str 0 end) trim-indicator)
                        (concat str (make-string length-diff 32)))))
    (when (or (facep cell-face) (and (listp cell-face) (not (null cell-face))))
      (set-text-properties 0 (length cell-string) `(face ,cell-face font-lock-face ,cell-face) cell-string))
    cell-string))

(defun viztab--face-or-anonymous-face-p (object)
  "Return t if OBJECT is a face or an anonymous face."
  (let ((known-face-attributes '(:family :foundry :width :height :weight :slant :foreground :distant-foreground :background :underline :overline :strike-through :box :inverse-video :stipple :font :inherit)))
    (or (facep object) (and (listp object) (not (null object)) (memq (car object) known-face-attributes)))))

(defun viztab--repeat (list-or-element length)
  "Repeat one element or a list of elements until LENGTH is reached.

IF LIST-OR-ELEMENT is a string this function returns a list of
length LENGTH where each element is LIST-OR-ELEMENT.

If LIST-OR-ELEMENT is a list this function concats
LIST-OR-ELEMENT until LENGTH is reached. For more information see
`viztab--repeat-list'."
  (if (or (stringp list-or-element) (viztab--face-or-anonymous-face-p list-or-element))
      (make-list length list-or-element)
    (viztab--repeat-list list-or-element length)))

(defun viztab--repeat-list (list length)
  "Repeat elements in LIST until LENGTH is reached.

If the length of the list LIST if it is longer than
LENGTH the result is a list containing only the first LENGTH
elements of LIST."
  (let* ((list-length (seq-length list))
         (repeat (/ length list-length))
         (rest (% length list-length)))
    (if (= list-length length)
        list
      (append (apply 'append (make-list repeat list)) (seq-take list rest)))))

(defun viztab--make-row-string (cell-strings column-start column-seperator column-end)
  "Create a row string from a list of CELL-STRINGS.

Returns the concatenated string \"< COLUMN-START > < CELL-STRINGS seperated by COLUMN-SEPERATOR > < COLUMN-END >\"."
  (let* ((interspaces (- (length cell-strings) 1))
         (column-seperators (viztab--repeat column-seperator interspaces)))
    (concat column-start (apply 'concat (seq-mapn 'concat cell-strings (append column-seperators '("")))) column-end)))

(cl-defgeneric viztab-update-visual-rows ()
  "Update the visual display of a `viztab' table rows.")

(cl-defmethod viztab-update-visual-rows ((table viztab) &optional widths)
  "Update the visual display of a `viztab' table rows."
  (let* ((splitted-rows (viztab-split-rows table))
         (widths (or widths (viztab--compute-column-widths splitted-rows)))
         (number-of-cells (length widths))
         (trim-indicators (viztab--repeat (oref table trim-indicator) number-of-cells))
         (column-seperator (oref table column-seperator))
         (column-start (oref table column-start))
         (column-end (oref table column-end))
         (column-faces (if (slot-boundp table 'column-face)
                           (viztab--repeat (oref table column-face) number-of-cells)
                         (make-list number-of-cells nil)))
         (cell-strings (seq-map #'(lambda (row) (seq-mapn 'viztab--make-cell-string row widths trim-indicators column-faces)) splitted-rows))
         (row-strings (seq-map #'(lambda (row-strings) (viztab--make-row-string row-strings column-start column-seperator column-end)) cell-strings)))
    (oset table column-widths widths)
    (oset table visual-rows row-strings)))

(cl-defgeneric viztab--append-table-to-buffer ()
  "Append a table to a buffer.")

(cl-defmethod viztab--append-table-to-buffer ((table viztab) buffer-or-name)
  "Append string representation of TABLE to BUFFER-OR-NAME."
  (let ((table-string (mapconcat 'identity (oref table visual-rows) "\n")))
    (with-current-buffer (get-buffer-create buffer-or-name)
      (save-excursion
        (goto-char (point-max))
        (insert table-string)))))

(cl-defgeneric viztab--write-table-to-buffer ()
  "Write a table to a buffer replacing its previous content.")

(cl-defmethod viztab--write-table-to-buffer ((table viztab) buffer-or-name)
  "Write a TABLE to a BUFFER-OR-NAME replacing its previous
  content."
  (with-current-buffer (get-buffer-create buffer-or-name)
    (erase-buffer)
    (viztab--append-table-to-buffer table buffer-or-name)))

(defun viztab--new-table-output-buffer ()
  "Create a new buffer for displaying tables."
  (generate-new-buffer "*viztab table*"))

(cl-defgeneric viztab-show-table ()
  "Show table in current window.")

(cl-defmethod viztab-show-table ((table viztab))
  "Show TABLE in current window."
  (interactive "STable to show: ")
  (if (not (viztab-p table))
      (error "The selected variable is not assigned to a viztab table")
    (let ((buffer (viztab--new-table-output-buffer)))
      (viztab--write-table-to-buffer table buffer)
      (switch-to-buffer buffer))))

(cl-defgeneric viztab-show-table-other-window ()
  "Show table in other window.")

(cl-defmethod viztab-show-table-other-window ((table viztab))
  "Show TABLE in other window."
  (interactive "STable to show: ")
  (if (not (viztab-p table))
      (error "The selected variable is not assigned to a viztab table")
    (let ((buffer (viztab--new-table-output-buffer)))
      (viztab--write-table-to-buffer table buffer)
      (switch-to-buffer-other-window buffer))))

(provide 'viztab)
;;; viztab.el ends here
