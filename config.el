;;; editor/fmt/config.el -*- lexical-binding: t; -*-

(defvar-local +fmt-formatter #'indent-region
  "The formatter that is used by `+fmt/dwim'.
This is a function that takes either zero or two arguments BEG and END.
With zero arguments, it shall format the current buffer.
With two arguments, it shall format the current buffer from BEG to END.")

(use-package! format-all
  :commands (format-all-buffer)
  :init (setq-default +fmt-formatter #'format-all-buffer)
  :config
  (add-to-list 'debug-ignored-errors "^No supported formatters for ")
  (advice-add #'format-all--buffer-thunk :override #'+fmt-restrict-buffer-thunk-a)
  (advice-add #'format-all--run-chain :override #'+fmt-restrict-run-chain-a))

(when (featurep! +define)
  (use-package! reformatter :defer t))

(when (featurep! :tools lsp)
  (defun +fmt-lsp-format-buffer-or-region (&optional beg end)
    "Run `lsp-format-buffer' or `lsp-format-region' with BEG and END."
    (if (and (null beg) (null end))
        (lsp-format-buffer)
      (lsp-format-region beg end)))

  (defun +fmt-lsp-maybe-set-formatter-h ()
    "Change formatter to the language server if formatting is supported."
    (let ((buffer (lsp-feature? "textDocument/formatting"))
          (region (lsp-feature? "textDocument/rangeFormatting")))
      (if (and buffer region)
          (setq +fmt-formatter #'+fmt-lsp-format-buffer-or-region)
        (when buffer (setq +fmt-formatter #'lsp-format-buffer))
        (when region (setq +fmt-formatter #'lsp-format-region)))))

  (add-hook 'lsp-mode-hook #'+fmt-lsp-maybe-set-formatter-h))
