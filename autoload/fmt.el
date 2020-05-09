;;; editor/fmt/autoload.el -*- lexical-binding: t; -*-

(defun +fmt--classify (fmt)
  "Test for the arities of FMT and return a cons (B . R).
If B is non-nil, FMT can be used with no arguments.
If R is non-nil, FMT can be used with two arguments."
  (let* ((arity (func-arity fmt))
         (min (car arity))
         (max (cdr arity))
         (buffer (zerop min))
         (region (and (<= min 2)
                      (or (symbolp max) (>= max 2)))))
    (unless (or buffer region)
      (error "Wrong formatter arity: %s, %s" fmt arity))
    (cons buffer region)))

(defun +fmt--current-indentation ()
  "Return the current general indentation."
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

(defmacro +fmt--narrowed-to-region (beg end &rest body)
  "Execute BODY rigidly narrowed to the region between BEG and END."
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
           (+fmt--save-indentation
            (cl-letf (((symbol-function 'widen) #'ignore))
              ,@body)))))))

;;;###autoload
(defun +fmt/buffer (&optional fmt)
  "Format the current buffer with FMT or `+fmt-formatter'."
  (interactive)
  (if (car (+fmt--classify fmt)) (funcall fmt)
    (funcall fmt (point-min) (point-max))))

;;;###autoload
(defun +fmt/region (beg end &optional fmt)
  "Format the current region with FMT or `+fmt-formatter'."
  (interactive "r")
  (if (cdr (+fmt--classify fmt)) (funcall fmt beg end)
    (if (and (eq beg (point-min))
             (eq end (point-max)))
        (funcall fmt)
      (+fmt--narrowed-to-region beg end (funcall buf-fn)))))

;;;###autoload
(defun +fmt/dwim ()
  "Format the current buffer or region."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+fmt/region
     #'+fmt/buffer)))
