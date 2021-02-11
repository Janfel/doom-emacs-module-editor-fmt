;;; autoload/fmt-prettier.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar prettier-config-file nil
  "The configuration file for `prettier-format-buffer'.

If this is nil, Prettier will find the correct file on it’s own.
If this is a string, it will override Prettier’s automatic detection.
If this is the symbol “none”, Prettier will not search for a config file.")

(defvar-local prettier-format-parser nil
  "The parser that `prettier-format-region' should use.")

(defvar prettier-format-parser-alist
  '(;; HTML
    (html-mode       . "html")
    (mhtml-mode      . "html")
    (web-mode        . "html")
    (markdown-mode   . "markdown")
    ;; CSS
    (css-mode        . "css")
    (scss-mode       . "scss")
    (less-css-mode   . "less")
    ;; JavaScript
    (js-mode         . "babel")
    (js2-mode        . "babel")
    (js3-mode        . "babel")
    ;; TypeScript
    (typescript-mode . "typescript")
    ;; PHP
    (php-mode        . "php")
    ;; JSON
    (json-mode       . "json")
    (yaml-mode       . "yaml")
    ;; GraphQL
    (graphql-mode    . "graphql")))

(defun prettier-compute-args ()
  "Compute arguments for `prettier-format-region'."
  (let ((parser (or prettier-format-parser
                    (cdr (assq major-mode prettier-format-parser-alist)))))
    (nconc
     (and buffer-file-name (list "--stdin-filepath" buffer-file-name))
     (and parser           (list "--parser"         parser))
     (when prettier-config-file
       (cond ((eq prettier-config-file 'none) (list "--no-config"))
             ((file-readable-p prettier-config-file)
              (list "--config" prettier-config-file))))
     (list
      ;; TODO: Use --cursor-offset --range-start --range-end.
      (if indent-tabs-mode "--use-tabs" "--no-use-tabs")
      "--print-width" (number-to-string fill-column)
      "--tab-width"   (number-to-string standard-indent)))))

;;;###autoload (autoload 'prettier-format-buffer "autoload/fmt-prettier" nil t)
;;;###autoload (autoload 'prettier-format-region "autoload/fmt-prettier" nil t)
(+fmt-define prettier
    :program "prettier"
    :args (prettier-compute-args))
