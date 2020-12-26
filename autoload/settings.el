;;; editor/fmt/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +fmt-combine (buffer-fn region-fn)
  "Combine a BUFFER-FN with a REGION-FN.
BUFFER-FN is a function that takes no arguments.
REGION-FN is a function that takes two arguments.
Return a function that can take zero or two arguments and calls
either BUFFER-FN or REGION-FN accordingly.
The resulting function can be used as value of `+fmt-formatter'."
  (lambda (&optional beg end)
    (if (eq beg end)
        (funcall buffer-fn)
      (funcall region-fn beg end))))
