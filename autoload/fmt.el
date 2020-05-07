;;; editor/fmt/autoload.el -*- lexical-binding: t; -*-

(defvar fmt/formatter)

(defun fmt--normalize (&optional fmt)
  "Return FMT or `fmt/formatter' as a cons (BUF-FN . REG-FN)."
  (unless fmt (setq fmt fmt/formatter))
  (cond
   ((not fmt) (error "No formatter"))

   ((functionp fmt)
    (cl-destructuring-bind (min . max) (func-arity fmt)
      (let ((fmt-cons (cons nil nil)))
        ;; Can `fmt' be used as buffer formatter?
        (when (zerop min) (setf (car fmt-cons) fmt))
        ;; Can `fmt' be used as region formatter?
        (when (and (<= min 2) (or (symbolp max) (>= max 2)))
          (setf (cdr fmt-cons) fmt))
        (if (or (car fmt-cons) (cdr fmt-cons)) fmt-cons
          (error "Wrong formatter arity: %s, %s, %s" fmt min max)))))

   ((consp fmt)
    (if (or (car fmt) (cdr fmt)) fmt
      (error "Invalid formatter: (nil . nil)")))

   (t (error "Invalid formatter: %s" fmt))))

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
  (cl-destructuring-bind (buffer-fn . region-fn) (fmt--normalize fmt)
    (if buffer-fn
        (funcall buffer-fn)
      (funcall region-fn (point-min) (point-max)))))

;;;###autoload
(defun fmt/region (beg end &optional fmt)
  "Format the current region with FMT or `fmt/formatter'."
  (interactive "r")
  (cl-destructuring-bind (buffer-fn . region-fn) (fmt--normalize fmt)
    (if (and (eq beg (point-min)) (eq end (point-max)))
        (funcall buffer-fn)
      (if region-fn (funcall region-fn beg end)
        (save-excursion
          (save-restriction
            ;; Normalize the region to use full lines.
            (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
            (setq beg (max beg (point)))
            (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
            (setq end (min end (point)))

            (narrow-to-region beg end)
            (fmt--save-indentation
             (cl-letf (((symbol-function 'widen) #'ignore))
               (funcall buffer-fn)))))))))

;;;###autoload
(defun fmt/dwim ()
  "Format the current buffer or region."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'fmt/region
     #'fmt/buffer)))
