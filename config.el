;;; editor/fmt/config.el -*- lexical-binding: t; -*-

(defvar fmt/formatter
  (if (featurep! +format-all)
      #'format-all-buffer
    #'indent-region)
  "The formatter that is used by `fmt/dwim'.
This is a function FN that takes either zero or two arguments.
If FN is called with zero arguments, it shall format the current buffer.
If FN is called with two arguments BEG and END, it shall
format the region of the current buffer specified by BEG and END.")

(use-package! format-all
  :when (featurep! +format-all)
  :commands (format-all-buffer))

(when (featurep! :tools lsp)
  (defun +fmt-lsp-mode-maybe-set-formatter-h ()
    "Change formatter to `lsp-format' if the language server supports formatting."
    (let ((buffer (lsp-feature? "textDocument/formatting"))
          (region (lsp-feature? "textDocument/rangeFormatting")))
      (if (and buffer region)
          (setq fmt/formatter (cons #'lsp-format-buffer #'lsp-format-region))
        (when buffer (setq fmt/formatter #'lsp-format-buffer))
        (when region (setq fmt/formatter #'lsp-format-region)))))
  (add-hook 'lsp-mode-hook #'+fmt-lsp-mode-maybe-set-formatter-h))
