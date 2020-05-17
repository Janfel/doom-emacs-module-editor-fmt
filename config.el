;;; editor/fmt/config.el -*- lexical-binding: t; -*-

(defvar-local +fmt-formatter #'indent-region
  "The formatter that is used by `+fmt/dwim'.
This is a function FN that takes (0 . 2) arguments BEG and END.
If both arguments to FN are nil or `eq', it shall format the current buffer.
If at least one argument to FN is not nil, it shall format the region
of the current buffer specified by (`or' BEG (`point-min'))
and (`or' END (`point-max')).")

(use-package! format-all
  :commands (format-all-buffer)
  :init (setq-default +fmt-formatter #'format-all-buffer)
  :config (advice-add #'format-all-buffer :around
                      #'+fmt--format-all-respect-narrowing-a))

(when (featurep! +define)
  (use-package! reformatter :defer t))

(when (featurep! :tools lsp)
  (defun +fmt-lsp-mode-maybe-set-formatter-h ()
    "Change formatter to `lsp-format' if the language server supports formatting."
    (let ((buffer (lsp-feature? "textDocument/formatting"))
          (region (lsp-feature? "textDocument/rangeFormatting")))
      (if (and buffer region)
          (setq +fmt-formatter (+fmt-combine #'lsp-format-buffer #'lsp-format-region))
        (when buffer (setq +fmt-formatter #'lsp-format-buffer))
        (when region (setq +fmt-formatter #'lsp-format-region)))))
  (add-hook 'lsp-mode-hook #'+fmt-lsp-mode-maybe-set-formatter-h))
