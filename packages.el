;; -*- no-byte-compile: t; -*-
;;; editor/fmt/packages.el

(package! format-all)

(when (featurep! +define)
  (package! reformatter))
