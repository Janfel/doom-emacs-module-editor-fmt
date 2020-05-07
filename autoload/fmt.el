;;; editor/fmt/autoload.el -*- lexical-binding: t; -*-

(defvar fmt/formatter)

(defun fmt--classify (fmt)
  (unless fmt (error "No formatter specified"))
  (let* ((arity (func-arity fmt))
         (min (car arity))
         (max (cdr arity))
         (buffer (zerop min))
         (region (and (<= min 2)
                      (or (symbolp max) (>= 2 max)))))
    (unless (or buffer region)
      (error "Wrong formatter arity: %s, %s, %s" fmt min max))
    (cons buffer region)))

(defun fmt--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n\r")
    (current-indentation)))

(defmacro fmt--save-indentation (&rest body)
  (let ((indent (make-symbol "indent")))
    `(let ((,indent (fmt--current-indentation)))
       (indent-rigidly (point-min) (point-max) (- ,indent))
       ,@body
       (indent-rigidly
        (point-min) (point-max)
        (max 0 (- ,indent (fmt--current-indentation)))))))

(defmacro fmt--narrowed-to-region (beg end &rest body)
  (let ((beg-sym (make-symbol "beg"))
        (end-sym (make-symbol "end")))
    `(let (,beg-sym ,end-sym)
       (save-excursion
         (save-restriction
           ;; Normalize the region to use full lines.
           (goto-char ,beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
           (setq ,beg-sym (max ,beg (point)))
           (goto-char ,end) (skip-chars-backward " \t\n\r") (end-of-line)
           (setq ,end-sym (min ,end (point)))

           (narrow-to-region ,beg-sym ,end-sym)
           (fmt--save-indentation
            (cl-letf (((symbol-function 'widen) #'ignore))
              ,@body)))))))

;;;###autoload
(defmacro fmt-defcombine! (name buf-fn reg-fn)
  (declare (indent defun))
  `(defun ,name (&optional beg end)
     "Reformats the current buffer or region from BEG to END.
If BEG or END is nil, `point-min' and `point-max' are used instead.
Suitable for direct use in `fmt/formatter'."
     (if (eq beg end) (,buf-fn)
       (,reg-fn (or beg (point-min)) (or end (point-max))))))

;;;###autoload
(defmacro fmt-define! (name &rest body)
  "Define the formatter NAME using `reformatter-define'.

This macro creates the functions `fmt|NAME-format-buffer'
and `fmt|NAME-format-region'.
It also creates a function called `fmt|NAME' that combines the above.
This function can be used as the value of `fmt/formatter'.
BODY is passed to `reformatter-define' unchanged, however the argument
:mode is set to nil by default."
  (declare (indent defun))
  (let ((short-name (intern (format "fmt|%s" name)))
        (long-name (intern (format "fmt|%s-format" name)))
        (name-buf (intern (format "fmt|%s-format-buffer" name)))
        (name-reg (intern (format "fmt|%s-format-region" name))))
  `(progn
     (reformatter-define ,long-name :mode nil ,@body)
     (fmakunbound ',long-name)
     (defun ,short-name (&optional beg end)
       "Reformats the current buffer or region from BEG to END.
If BEG or END is nil, `point-min' and `point-max' are used instead.
Suitable for direct use in `fmt/formatter'."
       (if (eq beg end) (,name-buf)
         (,name-reg (or beg (point-min)) (or end (point-max))))))))

;;;###autoload
(defun fmt/buffer (&optional fmt)
  "Format the current buffer with FMT or `fmt/formatter'."
  (interactive)
  (let* ((fmt (or fmt fmt/formatter))
         (buffer? (car (fmt--classify fmt))))
    (if buffer?
        (funcall fmt)
      (funcall fmt (point-min) (point-max)))))

;;;###autoload
(defun fmt/region (beg end &optional fmt)
  "Format the current region with FMT or `fmt/formatter'."
  (interactive "r")
  (let* ((fmt (or fmt fmt/formatter))
         (region? (cdr (fmt--classify fmt))))
    (if region? (funcall fmt beg end)
      (if (and (eq beg (point-min)) (eq end (point-max)))
          (funcall fmt)
        (fmt--narrowed-to-region beg end (funcall fmt))))))

;;;###autoload
(defun fmt/dwim ()
  "Format the current buffer or region."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'fmt/region
     #'fmt/buffer)))
