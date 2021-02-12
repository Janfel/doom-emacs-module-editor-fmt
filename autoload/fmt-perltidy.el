;;; editor/fmt/autoload/fmt-perltidy.el -*- lexical-binding: t; -*-

;;; Perl::Tidy - Parses and beautifies Perl source
;;; https://metacpan.org/release/Perl-Tidy

(defvar +fmt-perltidy-program "perltidy"
  "The executable for `+fmt-perltidy-format-region'.")

(defvar +fmt-perltidy-args nil
  "List of additional arguments passed to `+fmt-perltidy-program'.")

(defvar +fmt-perltidy-config-file nil
  "The configuration file for `+fmt-perltidy-program'.")

(defun +fmt-perltidy-compute-args ()
  "Compute arguments passed to `+fmt-perltidy-program'."
  (nconc
   (when (and +fmt-perltidy-config-file (file-readable-p +fmt-perltidy-config-file))
     (list (concat "--profile=" +fmt-perltidy-config-file)))
   (list (if indent-tabs-mode "--tabs" "--notabs")
         "--indent-columns"      (number-to-string standard-indent)
         "--default-tabsize"     (number-to-string tab-width)
         "--maximum-line-length" (number-to-string fill-column)
         "--standard-output" "--standard-error-output")
   +fmt-perltidy-args))

;;;###autoload (autoload '+fmt-perltidy-format-buffer "editor/fmt/autoload/fmt-perltidy" nil t)
;;;###autoload (autoload '+fmt-perltidy-format-region "editor/fmt/autoload/fmt-perltidy" nil t)
(+fmt-define +fmt-perltidy
  :program +fmt-perltidy-program
  :args (+fmt-perltidy-compute-args))
