;;; editor/fmt/autoload/fmt-lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! :tools lsp)

(defun +fmt-lsp-format-buffer-or-region (&optional beg end)
  "Run `lsp-format-buffer' or `lsp-format-region' with BEG and END."
  (if (and (null beg) (null end))
      (lsp-format-buffer)
    (lsp-format-region beg end)))

(defun +fmt-eglot-format-region (beg end)
  "Fixed-arity wrapper for `eglot-format'."
  (eglot-format beg end))

;;;###autoload
(defun +fmt-lsp-maybe-set-formatter-h ()
  "Change formatter to the language server if formatting is supported."
  (cl-destructuring-bind (buffer region both)
      (cond ((bound-and-true-p lsp-managed-mode)
             (list (if (lsp-feature? "textDocument/formatting") #'lsp-format-buffer)
                   (if (lsp-feature? "textDocument/rangeFormatting") #'lsp-format-region)
                   #'+fmt-lsp-format-buffer-or-region-h))
            ((bound-and-true-p eglot--managed-mode)
             (list (if (eglot--server-capable :documentFormattingProvider) #'eglot-format-buffer)
                   (if (eglot--server-capable :documentRangeFormattingProvider) #'+fmt-eglot-format-region)
                   #'eglot-format)))
    (setf +fmt-formatter (if (and buffer region) both (or region buffer +fmt-formatter)))))
