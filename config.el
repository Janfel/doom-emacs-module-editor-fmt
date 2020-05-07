;;; editor/fmt/config.el -*- lexical-binding: t; -*-

(defvar fmt/formatter
  (if (featurep! +format-all)
      #'format-all-buffer
    #'indent-region)
  "The formatter that is used by `fmt/dwim'.
This is either a cons (BUF-FN . REG-FN) or a function FN.
BUF-FN is a function that takes no arguments.
REG-FN is a function that takes two arguments.
FN is a function that takes zero or two arguments.")

(use-package! reformatter
  :commands (reformatter-define))

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
