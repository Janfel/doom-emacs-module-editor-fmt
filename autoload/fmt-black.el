;;; editor/fmt/autoload/fmt-black.el -*- lexical-binding: t; -*-

;;; Black - The Uncompromising Code Formatter
;;; https://github.com/psf/black

(defvar +fmt-black-program "black"
  "The executable for `+fmt-black-format-region'.")

(defvar +fmt-black-args nil
  "List of additional arguments passed to `+fmt-black-program'.")

(defvar +fmt-black-config-file nil
  "The configuration file for `+fmt-black-program'.")

(defun +fmt-black-compute-args ()
  "Compute arguments for `black-format-region'."
  (nconc
   (when (and +fmt-black-config-file (file-readable-p +fmt-black-config-file))
     (list "--config" +fmt-black-config-file))
   ;; Comes in the next version. Makes the next line obsolete.
   ;; (when buffer-file-name (list "--stdin-filename" buffer-file-name))
   (when (string-suffix-p ".pyi" (or buffer-file-name "") t) '("--pyi"))
   (list "--line-length" (number-to-string fill-column) "--quiet" "-")
   +fmt-black-args))

;;;###autoload (autoload '+fmt-black-format-buffer "editor/fmt/autoload/fmt-black" nil t)
;;;###autoload (autoload '+fmt-black-format-region "editor/fmt/autoload/fmt-black" nil t)
(+fmt-define +fmt-black
  :program +fmt-black-program
  :args (+fmt-black-compute-args))
