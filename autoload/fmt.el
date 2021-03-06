;;; editor/fmt/autoload/fmt.el -*- lexical-binding: t; -*-

(require 'find-func)

(defun +fmt--indirect-function-advised-original (object)
  "Return the conceptual, advice-free function binding of OBJECT."
  ;; TODO: Fix `nil' error on first call.
  (require 'nadvice)
  (catch 'break
    (while object
      (setq object
            (cond ((advice--p object) (advice--cd*r object))
                  ((autoloadp object) (autoload-do-load object))
                  ((symbolp object)   (indirect-function object))
                  ((functionp object) (throw 'break object)))))))

(defun +fmt--formatter-p (func &optional error)
  "Test for the arities of FUNC and return a cons (B . R) or nil.
If B is non-nil, FUNC can be used with no arguments.
If R is non-nil, FUNC can be used with two arguments.
If the return value is nil, FUNC can not be used as a formatter.
If the optional argument ERROR is t, the function will
signal a `user-error' when it would return nil."
  ;; Guard against macros and special forms.
  (unless (functionp func) (signal 'wrong-type-argument (list #'functionp func)))
  (cl-destructuring-bind (min . max)
      (func-arity (+fmt--indirect-function-advised-original func))
    (let ((buffer (zerop min))
          (region (and (<= min 2) (or (eq max 'many) (<= 2 max)))))
      (cond ((or buffer region) (cons buffer region))
            (error (user-error "Unsupported formatter arity: %s, %s"
                               func (cons min max)))))))

(defun +fmt--current-indentation ()
  "Return the current general indentation."
  ;; TODO: Look at the minimum of all lines
  ;; instead of just the first non-blank line.
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n\r")
    (current-indentation)))

(defmacro +fmt--save-indentation (&rest body)
  "Save the current general indentation; execute BODY; restore the indentation."
  (let ((indent (make-symbol "indent")))
    `(let ((,indent (+fmt--current-indentation)))
       (indent-rigidly (point-min) (point-max) (- ,indent))
       ,@body
       (indent-rigidly
        (point-min) (point-max)
        (max 0 (- ,indent (+fmt--current-indentation)))))))

;;;###autoload
(defmacro +fmt-define (name &rest params)
  "Define NAME-format-buffer and NAME-format-region formatters.
This is a thin wrapper around `reformatter-define' that disables the creation
of NAME-on-save-mode and the NAME-format alias."
  (declare (indent defun))
  (let ((long-name (intern (format "%s-format" name))))
    `(progn
       (reformatter-define ,long-name :mode nil ,@params)
       (fmakunbound ',long-name))))

(defalias 'formatter-define! #'+fmt-define)

;;;###autoload
(defun +fmt/buffer (&optional fmt)
  "Format the current buffer with FMT or `+fmt-formatter'."
  (interactive "*")
  (unless fmt (setq fmt +fmt-formatter))
  (if (car (+fmt--formatter-p fmt 'error)) (funcall fmt)
    (funcall fmt (point-min) (point-max))))

;;;###autoload
(defun +fmt/region (beg end &optional fmt)
  "Format the current region with FMT or `+fmt-formatter'."
  (interactive "*r")
  (unless fmt (setq fmt +fmt-formatter))
  (if (cdr (+fmt--formatter-p fmt 'error)) (funcall fmt beg end)
    (if (or (= beg end) (and (= beg (point-min)) (= end (point-max))))
        (funcall fmt)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (+fmt--save-indentation
           (funcall fmt)))))))

;;;###autoload
(defun +fmt/dwim ()
  "Format the current buffer or region."
  (interactive "*")
  (call-interactively
   (if (doom-region-active-p)
       #'+fmt/region
     #'+fmt/buffer)))
