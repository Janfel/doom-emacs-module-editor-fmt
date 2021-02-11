;;; autoload/fmt-tidy.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar tidy-config-file nil
  "The configuration file for `tidy-format-buffer'.")

(defvar tidy-xml-mode-list '(nxml-mode)
  "A list of modes where `tidy-format-buffer' uses the “-xml” flag.")

(defun tidy-compute-args ()
  "Compute arguments for `tidy-format-buffer'."
  (when indent-tabs-mode (warn "HTML Tidy doesn’t support indent-tabs-mode"))
  (nconc
   (when (memq major-mode tidy-xml-mode-list) '("-xml"))
   (when (and tidy-config-file (file-readable-p tidy-config-file))
     (list "-config" tidy-config-file))
   (list
    "--quiet"            "yes"
    "--tidy-mark"        "no"
    "--indent"           "yes"
    "--indent-with-tabs" (if indent-tabs-mode "yes" "no")
    "--indent-spaces"    (if indent-tabs-mode "1" (number-to-string standard-indent))
    "--wrap"             (number-to-string fill-column)
    "--tab-size"         (number-to-string tab-width))))

;;;###autoload (autoload 'tidy-format-buffer "autoload/fmt-tidy" nil t)
;;;###autoload (autoload 'tidy-format-region "autoload/fmt-tidy" nil t)
(+fmt-define tidy
  :program "tidy"
  :args (tidy-compute-args)
  :exit-code-success-p (lambda (x) (memq x '(0 1))))
