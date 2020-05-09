;;; editor/fmt/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +fmt-combine (buffer-fn region-fn)
  "Combine a BUFFER-FN with a REGION-FN.

BUFFER-FN is a function that takes no arguments and formats the current buffer.
REGION-FN is a function that takes two arguments BEG and END and formats
the region of the current buffer specified by BEG and END.
The resulting function takes (0 . 2) arguments and calls
either BUFFER-FN or REGION-FN."
  (lambda (&optional beg end)
    (if (eq beg end)
        (funcall buffer-fn)
      (funcall region-fn beg end))))
