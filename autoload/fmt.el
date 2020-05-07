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
