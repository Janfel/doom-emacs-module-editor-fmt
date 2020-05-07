;; -*- no-byte-compile: t; -*-
;;; editor/fmt/packages.el

(package! reformatter)

(when (featurep! +format-all)
  (package! format-all))
