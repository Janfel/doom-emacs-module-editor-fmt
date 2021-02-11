;;; autoload/fmt-prettier.el -*- lexical-binding: t; -*-

;;; Prettier - Opinionated Code Formatter
;;; https://prettier.io/

(defvar +fmt-prettier-program "prettier"
  "The executable for `+fmt-prettier-format-region'.")

(defvar +fmt-prettier-args nil
  "List of additional arguments passed to `+fmt-prettier-program'.")

(defvar +fmt-prettier-config-file nil
  "The configuration file for `+fmt-prettier-program'.
If this is nil, Prettier will find the correct file on it’s own.
If this is a string, it will override Prettier’s automatic detection.
If this is the symbol “none”, Prettier will not search for a config file.")

(defvar +fmt-prettier-parser nil
  "The parser that `+fmt-prettier-format-region' should use.
Bind this variable locally to override `+fmt-prettier-parser-alist'.
For example:
  (setq-hook! 'flow-minor-mode-hook +fmt-prettier-parser \"flow\").")

(defvar +fmt-prettier-parser-alist
  '(;; HTML
    (html-mode       . "html")
    (mhtml-mode      . "html")
    (web-mode        . "html")
    ;; CSS
    (css-mode        . "css")
    (sass-mode       . "scss")
    (scss-mode       . "scss")
    (less-css-mode   . "less")
    ;; JavaScript
    (js-mode         . "babel")
    (js-jsx-mode     . "babel")
    (js2-mode        . "babel")
    (js3-mode        . "babel")
    (rjsx-mode       . "babel")
    ;; TypeScript
    (typescript-mode . "typescript")
    ;; Programming Languages
    (lua-mode        . "lua")
    (php-mode        . "php")
    (ruby-mode       . "ruby")
    (swift-mode      . "swift")
    (nxml-mode       . "xml")
    ;; Data Languages
    (json-mode       . "json")
    (yaml-mode       . "yaml")
    (toml-mode       . "toml")
    ;; Domain Specific Languages
    (graphql-mode    . "graphql")
    (haml-mode       . "haml")
    (markdown-mode   . "markdown")
    (pug-mode        . "pug")
    (solidity-mode   . "solidity-parse"))
  "Alist mapping major modes to `+fmt-prettier-parser' values.")

(defun +fmt-prettier-compute-args ()
  "Compute arguments for `+fmt-prettier-format-region'."
  (let ((parser (or +fmt-prettier-parser
                    (cdr (assq major-mode +fmt-prettier-parser-alist)))))
    (nconc
     (and buffer-file-name (list "--stdin-filepath" buffer-file-name))
     (and parser           (list "--parser"         parser))
     (when +fmt-prettier-config-file
       (cond ((eq +fmt-prettier-config-file 'none) (list "--no-config"))
             ((file-readable-p +fmt-prettier-config-file)
              (list "--config" +fmt-prettier-config-file))))
     (list
      (if indent-tabs-mode "--use-tabs" "--no-use-tabs")
      "--print-width" (number-to-string fill-column)
      "--tab-width"   (number-to-string standard-indent))
     +fmt-prettier-args)))

;;;###autoload (autoload '+fmt-prettier-format-buffer "autoload/fmt-prettier" nil t)
;;;###autoload (autoload '+fmt-prettier-format-region "autoload/fmt-prettier" nil t)
(+fmt-define +fmt-prettier
    :program +fmt-prettier-program
    :args (+fmt-prettier-compute-args))
