;;; autoload/fmt-shfmt.el -*- lexical-binding: t; -*-

;;; shfmt - Format shell programs
;;; https://github.com/mvdan/sh

(defvar +fmt-shfmt-program "shfmt"
  "The executable for `+fmt-shfmt-format-region'.")

(defvar +fmt-shfmt-args '("-sr")
  "List of additional arguments passed to `+fmt-shfmt-program'.")

(defun +fmt-shfmt-compute-args ()
  "Compute arguments passed to `+fmt-shfmt-program'."
  (nconc
   (and buffer-file-name (list "-filename" buffer-file-name))
   (when (and (derived-mode-p 'sh-mode) (bound-and-true-p sh-shell))
     (list "-ln" (symbol-name sh-shell)))
   (list "-i" (number-to-string (if indent-tabs-mode 0 standard-indent)))
   +fmt-shfmt-args))

;;;###autoload (autoload '+fmt-shfmt-format-buffer "autoload/fmt-shfmt" nil t)
;;;###autoload (autoload '+fmt-shfmt-format-region "autoload/fmt-shfmt" nil t)
(+fmt-define +fmt-shfmt
  :program +fmt-shfmt-program
  :args (+fmt-shfmt-compute-args))
