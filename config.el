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

(when (featurep! +format-all)
  (use-package! format-all
    :commands (format-all-buffer
               format-all-mode)))
