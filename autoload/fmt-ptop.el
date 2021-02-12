;;; editor/fmt/autoload/fmt-ptop.el -*- lexical-binding: t; -*-

;;; PTOP - Free Pascal Source Formatter
;;; https://freepascal.org/tools/ptop.html

(defvar +fmt-ptop-program "ptop"
  "The executable for `+fmt-ptop-format-region'.")

(defvar +fmt-ptop-args nil
  "List of additional arguments passed to `+fmt-ptop-program'.")

(defvar +fmt-ptop-config-file nil
  "The configuration file for `+fmt-ptop-program'.")

(defun +fmt-ptop-compute-args (input-file)
  "Compute arguments passed to `+fmt-ptop-program'."
  (nconc
   (when (and +fmt-ptop-config-file (file-readable-p +fmt-ptop-config-file))
     (list "-c" +fmt-ptop-config-file))
   (list "-i" (number-to-string standard-indent)
         "-l" (number-to-string fill-column))
   (copy-sequence +fmt-ptop-args)
   (list input-file input-file)))

;;;###autoload (autoload '+fmt-ptop-format-buffer "editor/fmt/autoload/fmt-ptop" nil t)
;;;###autoload (autoload '+fmt-ptop-format-region "editor/fmt/autoload/fmt-ptop" nil t)
(+fmt-define +fmt-ptop
  :program +fmt-ptop-program
  :args (+fmt-ptop-compute-args input-file)
  :stdin nil :stdout nil)
