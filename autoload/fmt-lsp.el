;;; editor/fmt/autoload/fmt-lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! :tools lsp)

(defun +fmt-lsp-format-buffer-or-region (&optional beg end)
  "Run `lsp-format-buffer' or `lsp-format-region' with BEG and END."
  (if (and (null beg) (null end))
      (lsp-format-buffer)
    (lsp-format-region beg end)))

;;;###autoload
(defun +fmt-lsp-maybe-set-formatter-h ()
  "Change formatter to the language server if formatting is supported."
  (let ((buffer (lsp-feature? "textDocument/formatting"))
        (region (lsp-feature? "textDocument/rangeFormatting")))
    (if (and buffer region)
        (setq +fmt-formatter #'+fmt-lsp-format-buffer-or-region)
      (when buffer (setq +fmt-formatter #'lsp-format-buffer))
      (when region (setq +fmt-formatter #'lsp-format-region)))))
