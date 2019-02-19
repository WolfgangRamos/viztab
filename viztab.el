;;; viztab.el --- Visualize (Data) Structures as tables

;;; Commentary:
;;
;;
;;; Code:
(require 'eieio)
(require 'cl-generic)

(defclass viztab ()
  ((data :initarg :data :type sequence)
   (column-seperator :initarg :column-seperator :type (or string list) :initform " ")
   (column-start :initarg :column-start :type string :initform "")
   (column-end :initarg :column-end :type string :initform "")
   (trim-indicator :initarg :trim-indicator :type string :initform "…")
   (column-face :initarg :column-face (or face list))
   (column-widths :initform nil)
   (splitter
    :initarg :splitter
    :type function
    :initform 'identity
    :documentation
    "A function to split a row object into individual cells. This function must resturn a sequence. This function is applied to each element or row-objects to generate the columns.")
   (visual-rows)))

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
         (trim-indicators (viztab--repeat (oref table :trim-indicator) number-of-cells))
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
      (error "The selected variable is not assigned to a viztab table.")
    (let ((buffer (viztab--new-table-output-buffer)))
      (viztab--write-table-to-buffer table buffer)
      (switch-to-buffer buffer))))

(cl-defgeneric viztab-show-table-other-window ()
  "Show table in other window.")

(cl-defmethod viztab-show-table-other-window ((table viztab))
  "Show TABLE in other window."
  (interactive "STable to show: ")
  (if (not (viztab-p table))
      (error "The selected variable is not assigned to a viztab table.")
    (let ((buffer (viztab--new-table-output-buffer)))
      (viztab--write-table-to-buffer table buffer)
      (switch-to-buffer-other-window buffer))))

(provide 'viztab)
;;; viztab.el ends here
