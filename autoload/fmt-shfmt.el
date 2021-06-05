;;; editor/fmt/autoload/fmt-shfmt.el -*- lexical-binding: t; -*-

;;; shfmt - Format shell programs
;;; https://github.com/mvdan/sh

(defvar +fmt-shfmt-program "shfmt"
  "The executable for `+fmt-shfmt-format-region'.")

(defvar +fmt-shfmt-args '("-sr")
  "List of additional arguments passed to `+fmt-shfmt-program'.")

(defvar +fmt-shfmt-shell-alist
  '((bats  bats)
    (mksh  pdksh mksh)
    (posix ash dash es jsh oash posix rpm sh wsh))
  "Alist mapping shfmt shells to lists of `sh-shell' shells.")

(defun +fmt-shfmt-compute-args ()
  "Compute arguments passed to `+fmt-shfmt-program'."
  (nconc
   (and buffer-file-name (list "-filename" buffer-file-name))
   (when (bound-and-true-p sh-shell)
     (when-let (shell (seq-some (lambda (c) (if (memq sh-shell (cdr c)) (car c)))
                                +fmt-shfmt-shell-alist))
       (list "-ln" (symbol-name shell))))
   (list "-i" (number-to-string (if indent-tabs-mode 0 standard-indent)))
   +fmt-shfmt-args))

;;;###autoload (autoload '+fmt-shfmt-format-buffer "editor/fmt/autoload/fmt-shfmt" nil t)
;;;###autoload (autoload '+fmt-shfmt-format-region "editor/fmt/autoload/fmt-shfmt" nil t)
(+fmt-define +fmt-shfmt
  :program +fmt-shfmt-program
  :args (+fmt-shfmt-compute-args))
