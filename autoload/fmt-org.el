;;; editor/fmt/autoload/fmt-org.el -*- lexical-binding: t; -*-
;;;###if (featurep! :lang org)

(require 'org)

;;;###autoload
(defun +org-format-dwim (&optional beg end)
  (interactive
   (if (doom-region-active-p)
       (list (doom-region-beginning) (doom-region-end))
     (list nil nil)))
  (unless (org-in-src-block-p)
    (user-error "Can’t format code outside src block"))
  (let ((element (org-element-at-point))
        (beg (or beg (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (let ((block-beg (progn
                         (goto-char (org-babel-where-is-src-block-head element))
                         (line-beginning-position 2)))
            (block-end (progn
                         (goto-char (org-element-property :end element))
                         (skip-chars-backward " \t\r\n")
                         (line-beginning-position)))
            (buf (current-buffer))
            (mode-fn (org-src-get-lang-mode
                      (org-element-property :language element))))
        (when (org-src--edit-buffer block-beg block-end)
          (user-error "This src block is currently being edited"))
        (let ((beg (max beg block-beg))
              (end (min end block-end))
              (indbuf (make-indirect-buffer
                       buf (generate-new-buffer-name " *FMT Org Mode Temp*"))))
          (unwind-protect
              (with-current-buffer indbuf
                (narrow-to-region block-beg block-end)
                (funcall mode-fn)
                (+fmt/region beg end))
            (kill-buffer indbuf)
            (font-lock-flush)))))))
