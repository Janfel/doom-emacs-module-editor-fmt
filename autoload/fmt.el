;;; editor/fmt/autoload.el -*- lexical-binding: t; -*-

;; {{{ Normalizing the formatter.
(defun fmt--classify (fmt)
  (let* ((arity (func-arity fmt))
         (min (car arity))
         (max (cdr arity))
         (buffer (zerop min))
         (region (and (<= min 2)
                      (or (symbolp max) (>= 2 max)))))
    (unless (or buffer region)
      (error "Wrong formatter arity: %s, %s, %s" fmt min max))
    (cons buffer region)))

(defun fmt--normalize (&optional fmt)
  (unless fmt (setq fmt fmt/formatter))
  (cond ((null fmt) (error "No formatter specified"))
        ((consp fmt) (if (or (car fmt) (cdr fmt)) fmt
                       (error "Invalid formatter: (nil . nil)")))
        ((functionp fmt) (let ((arity (fmt--classify fmt)))
                           (cons (and (car arity) fmt)
                                 (and (cdr arity) fmt))))
        (t (error "Invalid formatter: %s" fmt))))
;; }}}
;; {{{ Narrowing to region.
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
;; }}}

;;;###autoload
(defmacro fmt-define! (name &rest body)
  "Define the formatter NAME using `reformatter-define'.

This macro creates the functions
`fmt|NAME-format-buffer' and `fmt|NAME-format-region'.
It also creates a function called `fmt|NAME' to be
used as the value of `fmt/formatter'.
BODY is passed to `reformatter-define' unchanged,
however the argument :mode is set to nil by default."
  (declare (indent defun))
  (let ((short-name (intern (format "fmt|%s" name)))
        (long-name (intern (format "fmt|%s-format" name)))
        (name-reg (intern (format "fmt|%s-format-region" name))))
  `(progn
     (reformatter-define ,long-name :mode nil ,@body)
     (fmakunbound ',long-name)
     (defalias ',short-name #',name-reg))))

;;;###autoload
(defun fmt/buffer (&optional fmt)
  "Format the current buffer with FMT or `fmt/formatter'."
  (interactive)
  (cl-destructuring-bind (buf-fn . reg-fn) (fmt--normalize fmt)
    (if buf-fn (funcall buf-fn)
      (funcall reg-fn (point-min) (point-max)))))

;;;###autoload
(defun fmt/region (beg end &optional fmt)
  "Format the current region with FMT or `fmt/formatter'."
  (interactive "r")
  (cl-destructuring-bind (buf-fn . reg-fn) (fmt--normalize fmt)
    (if reg-fn (funcall reg-fn beg end)
      (if (and (eq beg (point-min)) (eq end (point-max)))
          (funcall buf-fn)
        (fmt--narrowed-to-region beg end (funcall buf-fn))))))

;;;###autoload
(defun fmt/dwim ()
  "Format the current buffer or region."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'fmt/region
     #'fmt/buffer)))
