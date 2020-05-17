;;; editor/fmt/autoload/+define.el -*- lexical-binding: t; -*-
;;;###if (featurep! +define)

;;;###autoload
(defmacro formatter-define! (name &rest params)
  "Define a reformatter command with NAME.

This is a thin wrapper around `reformatter-define' that disables the creation
of the NAME-on-save-mode and the NAME-format alias."
  (declare (indent defun))
  (let ((long-name (intern (format "%s-format" name))))
    `(progn
       (reformatter-define ,long-name :mode nil ,@params)
       (fmakunbound ',long-name))))
