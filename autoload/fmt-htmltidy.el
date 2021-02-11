;;; autoload/fmt-htmltidy.el -*- lexical-binding: t; -*-

;;; HTML Tidy - A tool to tidy down your HTML code to a clean style
;;; https://www.html-tidy.org/

(defvar +fmt-htmltidy-program "tidy"
  "The executable for `+fmt-htmltidy-format-region'.")

(defvar +fmt-htmltidy-args nil
  "List of additional arguments passed to `+fmt-htmltidy-program'.")

(defvar +fmt-htmltidy-config-file nil
  "The configuration file for `+fmt-htmltidy-program'.")

(defvar +fmt-htmltidy-xml-mode-list '(nxml-mode)
  "List of modes where `+fmt-htmltidy-format-buffer' uses the “-xml” flag.")

(defun tidy-compute-args ()
  "Compute arguments passed to `+fmt-htmltidy-program'."
  (nconc
   (when (memq major-mode +fmt-htmltidy-xml-mode-list) '("-xml"))
   (when (and +fmt-htmltidy-config-file (file-readable-p +fmt-htmltidy-config-file))
     (list "-config" +fmt-htmltidy-config-file))
   (list
    "--quiet"            "yes"
    "--tidy-mark"        "no"
    "--indent"           "yes"
    "--indent-with-tabs" (if indent-tabs-mode "yes" "no")
    "--indent-spaces"    (if indent-tabs-mode "1" (number-to-string standard-indent))
    "--wrap"             (number-to-string fill-column)
    "--tab-size"         (number-to-string tab-width))
   +fmt-htmltidy-args))

;;;###autoload (autoload '+fmt-htmltidy-format-buffer "autoload/fmt-htmltidy" nil t)
;;;###autoload (autoload '+fmt-htmltidy-format-region "autoload/fmt-htmltidy" nil t)
(+fmt-define +fmt-htmltidy
  :program +fmt-htmltidy-program
  :args (+fmt-htmltidy-compute-args)
  :exit-code-success-p (lambda (x) (memq x '(0 1))))
