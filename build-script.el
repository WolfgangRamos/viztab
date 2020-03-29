;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(progn (setq package-build-working-dir "C:/Users/wra/.emacs.d/personal/modules/melpa/working/")
       (setq package-build-archive-dir "C:/Users/wra/.emacs.d/personal/modules/melpa/packages/")
       (setq package-build-recipes-dir "C:/Users/wra/.emacs.d/personal/modules/melpa/recipes/")
       (setq package-build-stable nil)
       (setq package-build-write-melpa-badge-images t))

(require 'package-build)
(package-build-archive "viztab")
(package-install-file )
