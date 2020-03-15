;;; editor/fmt/autoload.el -*- lexical-binding: t; -*-

(defvar fmt/formatter)

(defun fmt--normalize (&optional fmt)
  "Return FMT or `fmt/formatter' as a cons (BUF-FN . REG-FN)."
  (unless fmt (setq fmt fmt/formatter))
  (unless fmt (error "No formatter"))
  (unless (or (functionp fmt)
              (and (consp fmt)
                   (functionp (car fmt))
                   (functionp (cdr fmt))))
    (error "Invalid formatter: %s" fmt))

  (unless (consp fmt)
    ;; Here fmt is a function.
    (cl-destructuring-bind (min . max) (func-arity fmt)
      (setq fmt (cons (and (zerop min) fmt)
                      (and (<= min 2)
                           (or (symbolp max) (>= max 2))
                           fmt)))))
  ;; Here fmt is a cons.
  (unless (or (car fmt) (cdr fmt))
    (error "Formatter has wrong arity: %s" fmt))
  ;; Return fmt.
  fmt)

;;;###autoload
(defun fmt/buffer ()
  ""
  (interactive)
  (cl-destructuring-bind (buffer-fn . region-fn) (fmt--normalize)
    (if buffer-fn
        (funcall buffer-fn)
      (funcall region-fn (point-min) (point-max)))))

;;;###autoload
(defun fmt/region (beg end)
  ""
  (interactive "r")
  (cl-destructuring-bind (buffer-fn . region-fn) (fmt--normalize)
    (if region-fn
        (funcall region-fn beg end)
      (funcall buffer-fn))))

;;;###autoload
(defun fmt/dwim ()
  ""
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'fmt/region
     #'fmt/buffer)))
