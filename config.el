;;; editor/fmt/config.el -*- lexical-binding: t; -*-

(defvar-local +fmt-formatter #'indent-region
  "The formatter that is used by `+fmt/dwim'.
This is a function that takes either zero or two arguments BEG and END.
With zero arguments, it shall format the current buffer from `point-min' to
`point-max'. It shall not call `widen' or any function operating outside the
restriction, such as `erase-buffer'.
With two arguments, it shall format the current buffer from BEG to END.")

(use-package! format-all
  :commands (format-all-buffer)
  :init (setq-default +fmt-formatter #'format-all-buffer)
  :config
  (add-to-list 'debug-ignored-errors "^No supported formatters for ")
  (advice-add #'format-all--buffer-thunk :override #'+fmt-restrict-buffer-thunk-a)
  (advice-add #'format-all--run-chain :override #'+fmt-restrict-run-chain-a))

(use-package! reformatter :defer t)

(when (featurep! :tools lsp)
  (add-hook 'lsp-mode-hook #'+fmt-lsp-maybe-set-formatter-h))

(when (featurep! :lang lua)
  (setq-hook! 'lua-mode-hook +fmt-formatter #'+fmt-luaformatter-format-region))

(when (featurep! :lang org)
  (setq-hook! 'org-mode-hook +fmt-formatter #'+org-format-dwim))

(setq-hook! '(perl-mode-hook cperl-mode-hook)
  +fmt-formatter #'+fmt-perltidy-format-region)

(when (featurep! :lang sh)
  (setq-hook! 'sh-mode-hook +fmt-formatter #'+fmt-shfmt-format-region))
