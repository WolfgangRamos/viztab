;;; viztab-demo.el --- Prelude mode configuration

;;; Commentary:
;;
;; Code to create a colorful rainbow table.

;;; Code:
(require 'viztab)

(defvar viztab-demo-screenshot-command nil
  "Path to screenshot shell command.")

(defvar viztab-demo-screenshot-command-args nil
  "List of arguments passed to `viztab-demo-screenshot-command'.")

;; Faces
(defface viztab-demo-rainbow-table-red '((t (:foreground "red"))) "Viztab rainbow table test face red.")
(defface viztab-demo-rainbow-table-orange '((t (:foreground "orange"))) "Viztab rainbow table test face orange.")
(defface viztab-demo-rainbow-table-yellow '((t (:foreground "yellow"))) "Viztab rainbow table test face yellow.")
(defface viztab-demo-rainbow-table-green '((t (:foreground "green"))) "Viztab rainbow table test face green.")
(defface viztab-demo-rainbow-table-blue '((t (:foreground "blue"))) "Viztab rainbow table test face blue.")
(defface viztab-demo-rainbow-table-indigo'((t (:foreground "DarkSlateBlue"))) "Viztab rainbow table test face DarkSlateBlue.")
(defface viztab-demo-rainbow-table-violet '((t (:foreground "violet"))) "Viztab rainbow table test face violet.")

(defun viztab-demo--create-rainbow-table ()
  "Create a colourful table for testing."
  (let ((column-start "<< ")
        (column-end " >>")
        (first-seperator " | ")
        (second-seperator " / "))
    (set-text-properties 0 (length column-start) `(face viztab-demo-rainbow-table-red font-lock-face viztab-demo-rainbow-table-red) column-start)
    (set-text-properties 0 (length column-end) `(face viztab-demo-rainbow-table-violet font-lock-face viztab-demo-rainbow-table-violet) column-end)
    (set-text-properties 0 (length first-seperator) `(face viztab-demo-rainbow-table-yellow font-lock-face viztab-demo-rainbow-table-yellow) first-seperator)
    (set-text-properties 0 (length second-seperator) `(face viztab-demo-rainbow-table-blue font-lock-face viztab-demo-rainbow-table-blue) second-seperator)
    (make-instance 'viztab-table-definition
                   :column-face '(viztab-demo-rainbow-table-orange
                                  viztab-demo-rainbow-table-green
                                  viztab-demo-rainbow-table-indigo)
                   :row-start column-start
                   :row-end column-end
                   :column-seperator `(,first-seperator ,second-seperator))))

(defun viztab-demo--make-rainbow-table-screenshot-and-exit ()
  "Create a colorful table, display it, make and save a screenshot."
  (let ((rainbow-table-def (viztab-demo--create-rainbow-table))
        (screenshot-command viztab-demo-screenshot-command)
        (screenshot-command-args viztab-demo-screenshot-command-args)
        (buffer (generate-new-buffer "*Viztab Rainbow Table Demo*"))
        exit-code
        error-output)
    (switch-to-buffer buffer)
    (viztab-insert-table '(("A" "table" "full") ("of" "pretty" "things")
                           ("Unicorns" "Fairies" "Leprechauns")
                           ("Rainbows" "Nyan Cat" "etc."))
                         rainbow-table-def)
    (sit-for 0.5)
    (with-temp-buffer
      (setq exit-code (apply 'call-process screenshot-command nil (current-buffer) t screenshot-command-args))
      (when (> exit-code 0)
        (setq error-output (buffer-string))))
    (if (> exit-code 0)
        (insert (format "\n\nError while calling `%s %s'" screenshot-command (concat screenshot-command-args)) "\n\n" error-output)
      (kill-emacs))))

(provide 'viztab-demo)
;;; viztab-demo.el ends here
