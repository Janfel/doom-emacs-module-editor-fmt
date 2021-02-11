;;; autoload/fmt-perltidy.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar perltidy-config-file nil
  "The configuration file for `perltidy-format-region'.")

(defun perltidy-compute-args ()
  "Compute arguments for `perltidy-format-region'."
  (nconc
   (when (and perltidy-config-file (file-readable-p perltidy-config-file))
     (list (format "--profile=%s" perltidy-config-file)))
   (list
    (if indent-tabs-mode "--tabs" "--notabs")
    "--indent-columns"      (number-to-string standard-indent)
    "--default-tabsize"     (number-to-string tab-width)
    "--maximum-line-length" (number-to-string fill-column)
    "--standard-output" "--standard-error-output")))

;;;###autoload (autoload 'perltidy-format-buffer "autoload/fmt-perltidy" nil t)
;;;###autoload (autoload 'perltidy-format-region "autoload/fmt-perltidy" nil t)
(+fmt-define perltidy
  :program "perltidy"
  :args (perltidy-compute-args))
