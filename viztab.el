;;; viztab.el --- Visualize (Data) Structures as tables -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Wolfgang Ramos

;; Author: Wolfgang Ramos <wolfgang.ramos@gmail.com>
;; Version: 1.0
;; Package-Version: 20190404.0432
;; Maintainer: Wolfgang Ramos <wolfgang.ramos@gmail.com>
;; URL: https://github.com/WolfgangRamos/viztab
;; Package-Requires: ((emacs "25.1") (cl-generic "0.3"))

;; This file is not part of GNU Emacs.

;; This file is distributed unter the GNU Public License v3.0 or
;; later.

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

;; The most simple way to format one of these structures as table is:

;; (viztab-table <structure>)

;; e.g.:

;; (viztab-table '(("Foo" "FooBar") ("ABCDEF" "FGHIJKLM"))).

;; The formatting result can be further adjusted by:

;; - Customizing how elements of your list or vector structure
;;   (i.e. row elements) are transformed into lists of strings (i.e.
;;   cells). E.g. you could use the first and third element of
;;   each nested list and omit the second.

;; - Customizing the column seperator, as well as the row start and
;;   end strings and their faces.

;; - Setting maximum columns widths (cell strings are trimmed/padded
;;   accordingly).

;; - Customizing the trim indicator that is appended to a trimmed
;;   string to inform the user that the cell string was trimmed.

;; - Add faces to columns.

;; For more information refere to the documentation of `viztab-table'
;; and `viztab-table-definition'.

;; A demonstration of all features can be found under URL
;; `https://github.com/WolfgangRamos/viztab'

;;; Change Log:

;; - Version 1.0
;;   - created class viztab
;;   - created functions for creating cell and row strings
;;   - created test cases

;;; Code:

(require 'eieio)
(require 'cl-generic)
(require 'seq)

(defclass viztab-table-definition ()
  ((column-widths
    :initform nil
    :type list
    :initarg :column-widths
    :documentation
    "A list of integers. Each element of this specifies a column
    width in number of characters.")
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
   (row-start
    :initarg :row-start
    :type string
    :initform ""
    :documentation
    "The string used as the left outer border of the left most
    cell of each row. Text properties within this string are
    respected.")
   (row-end
    :initarg :row-end
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
    :initarg :column-face
    :type (or face list)
    :documentation
    "The named or anonymous face that should be applied to each
    column or a list of named or anonymous faces applied to the
    columns. In the latter case the list elements are repeated if
    the number of columns exceeds the number of given faces. Vice
    versa, if the number of given faces exceeds the number of
    columns the superfluous faces at the lists end are ignored.")))

(defun viztab--maybe-split-row-objects (row-objects &optional splitter-function)
  "Split ROW-OBJECTS into columns using SPLITTER-FUNCTION.

If SPLITTER-FUNCTION is nil return ROW-OBJECTS, else map
SPLITTER-FUNCTION over ROW-OBJECTS."
  (if (functionp splitter-function)
      (seq-map splitter-function row-objects)
    row-objects))

(cl-defgeneric viztab--get-or-compute-column-widths (rows (format viztab-table-definition))
  "Return column widths from FORMAT or compute them from the row data in ROWS.")

(cl-defmethod viztab--get-or-compute-column-widths (rows (format viztab-table-definition))
  "Return FORMATs `column-widths' slot value if it is bound and not nil.
Otherwise compute the column widths from the data in ROWS.

The elements of FORMATs `column-widths' are cyclically repeated
if necessary to match the number of columns in ROWS
see `viztab--cycle-repeat'. Note that the number of columns in
ROWS is only determined heuristically by looking at the number of
elements in the first element of ROWS."
  (let ((column-widths (and (slot-boundp format 'column-widths)
                            (not (null (oref format column-widths)))
                            (oref format column-widths)))
        (number-of-columns (if (not (seq-empty-p rows))
                               (seq-length (seq-elt rows 0))
                             0)))
    (if column-widths
        (if (and column-widths (= (length column-widths) number-of-columns))
            column-widths
          (viztab--cycle-repeat column-widths number-of-columns))
      (viztab--compute-column-widths rows))))

(defun viztab--compute-column-widths (rows)
  "Compute column widths required to fit the data in ROWS."
  (let ((column-widths (and (seqp rows)
                            (not (seq-empty-p rows))
                            (make-list (seq-length (seq-elt rows 0)) 0))))
    (seq-reduce #'(lambda (maximum-widths row) (seq-mapn 'max maximum-widths (seq-map 'length row))) rows column-widths)))

(defun viztab--make-cell-string (str length trim-indicator &optional cell-face)
  "Create a cell string by trimming STR to LENGTH.
If the length of STR exceeds LENGTH then STR is trimmed so that
the result of appending TRIM-INDICATOR to the trimmed STR results
in a string of length LENGTH. If LENGTH exceeds the length of
STR, STR is padded on the right side with spaces to match the
reqested LENGTH.

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

IF LIST-OR-ELEMENT is a string, a named or anonymous face, this
function returns a list of length LENGTH where each element is
LIST-OR-ELEMENT.

If LIST-OR-ELEMENT is a list this function concats
LIST-OR-ELEMENT until LENGTH is reached. For more information see
`viztab--cycle-repeat'."
  (if (or (stringp list-or-element) (viztab--face-or-anonymous-face-p list-or-element))
      (make-list length list-or-element)
    (viztab--cycle-repeat list-or-element length)))

(defun viztab--cycle-repeat (list length)
  "Cyclically repeat elements in LIST until LENGTH is reached.

If length of LIST is greater than LENGTH, return the first LENGTH
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

(defun viztab--table-definition-or-default (table-definition)
  "Return TABLE-DEFINITION or a default `viztab-table-definition'.

If TABLE-DEFINITION is nil return a default
`viztab-table-definition'. If TABLE-DEFINITION is not nil, throw
an error if it is *not* an instance of `viztab-table-definition';
otherwise return TABLE-DEFINITION."
  (or (and (viztab-table-definition-p table-definition) table-definition)
      (and (listp table-definition) (not (null table-definition)) (apply 'make-instance (cons 'viztab-table-definition table-definition)))
      (make-instance 'viztab-table-definition)))

;;;###autoload
(defun viztab-table (row-objects &optional table-definition object-to-cells)
  "Format ROW-OBJECTS as table.

ROW-OBJECTS must be either a list or a vector. TABLE-DEFINITION
must be a `viztab-table-definition' or a plist. If it is a plist
it is used as INITARGS to in a call to `make-instance' creating a
`viztab-table-definition' instance. OBJECT-TO-CELLS can be used
to transform the objects in ROW-OBJECTS before displaying them in
a table. If OBJECT-TO-CELLS is non-nil, it is (non-recursively)
applied to each element of ROW-OBJECTS to generate the cell
values that should be displayed. I.e. OBJECT-TO-CELLS must be a
function that accepts one argument and returns a list or a vector
of strings. If OBJECT-TO-CELLS is nil, each element of
ROW-OBJECTS is assumend to be a list or a vector of cells that
should be displayed as-is."
  (let* ((table-def (viztab--table-definition-or-default table-definition))
         (rows (viztab--maybe-split-row-objects row-objects object-to-cells))
         (widths (viztab--get-or-compute-column-widths rows table-def))
         (number-of-columns (length widths))
         (trim-indicators (viztab--repeat (oref table-def trim-indicator) number-of-columns))
         (column-seperator (oref table-def column-seperator))
         (row-start (oref table-def row-start))
         (row-end (oref table-def row-end))
         (column-faces (if (slot-boundp table-def 'column-face)
                           (viztab--repeat (oref table-def column-face) number-of-columns)
                         (make-list number-of-columns nil)))
         (cell-strings (and (> number-of-columns 0)
                            (seq-map #'(lambda (row) (seq-mapn 'viztab--make-cell-string row widths trim-indicators column-faces)) rows)))
         (row-strings (seq-map #'(lambda (cells) (viztab--make-row-string cells row-start column-seperator row-end)) cell-strings)))
    row-strings))

;;;###autoload
(defun viztab-insert-table (row-objects &optional table-definition object-to-cells)
  "Format ROW-OBJECTS as table and insert result into current buffer.

For information on parameters TABLE-DEFINITION and OBJECT-TO-CELLS see `viztab-table'"
  (let* ((table (viztab-table row-objects table-definition object-to-cells))
         (table-string (mapconcat 'identity table "\n")))
    (insert table-string)))

(provide 'viztab)
;;; viztab.el ends here
