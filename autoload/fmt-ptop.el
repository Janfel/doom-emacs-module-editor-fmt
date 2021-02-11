;;; autoload/fmt-ptop.el -*- lexical-binding: t; -*-

;;; PTOP - Free Pascal Source Formatter
;;; https://freepascal.org/tools/ptop.html

(defvar +fmt-ptop-program "ptop"
  "The executable for `+fmt-ptop-format-region'.")

(defvar +fmt-ptop-args nil
  "List of additional arguments passed to `+fmt-ptop-program'.")

(defvar +fmt-ptop-config-file nil
  "The configuration file for `+fmt-ptop-program'.")

;; TODO: Use :stdin/:stdout instead of /dev/ on Windows.
(defun +fmt-ptop-compute-args ()
  "Compute arguments passed to `+fmt-ptop-program'."
  (when IS-WINDOWS (error "PTOP can't be used on Windows"))
  (nconc
   (when (and +fmt-ptop-config-file (file-readable-p +fmt-ptop-config-file))
     (list "-c" +fmt-ptop-config-file))
   (list
    "-i" (number-to-string standard-indent)
    "-l" (number-to-string fill-column)
    "/dev/stdin" "/dev/stdout")
   +fmt-ptop-args))

;;;###autoload (autoload '+fmt-ptop-format-buffer "autoload/fmt-ptop" nil t)
;;;###autoload (autoload '+fmt-ptop-format-region "autoload/fmt-ptop" nil t)
(+fmt-define +fmt-ptop
  :program +fmt-ptop-program
  :args (+fmt-ptop-compute-args))
