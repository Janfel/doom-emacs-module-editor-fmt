;;; editor/fmt/autoload/fmt-luaformatter.el -*- lexical-binding: t; -*-

;;; LuaFormatter - Code formatter for Lua
;;; https://github.com/Koihik/LuaFormatter

(defvar +fmt-luaformatter-program "lua-format"
  "The executable for `+fmt-luaformatter-format-region'.")

(defvar +fmt-luaformatter-config-file nil
  "The configuration file for `+fmt-luaformatter-format-region'.")

(defun +fmt-luaformatter-compute-args ()
  "Compute arguments for `+fmt-luaformatter-format-region'."
  (let ((indent (if indent-tabs-mode 1 standard-indent)))
    (nconc
     (when (and +fmt-luaformatter-config-file
                (file-readable-p +fmt-luaformatter-config-file))
       (list "--config" +fmt-luaformatter-config-file))
     (list (if indent-tabs-mode "--use-tab" "--no-use-tab")
           "--column-limit"               (number-to-string fill-column)
           "--indent-width"               (number-to-string indent)
           "--continuation-indent-width"  (number-to-string indent)
           "--tab-width"                  (number-to-string tab-width)))))

;;;###autoload (autoload '+fmt-luaformatter-format-buffer "editor/fmt/autoload/fmt-luaformatter" nil t)
;;;###autoload (autoload '+fmt-luaformatter-format-region "editor/fmt/autoload/fmt-luaformatter" nil t)
(+fmt-define +fmt-luaformatter
  :program +fmt-luaformatter-program
  :args (+fmt-luaformatter-compute-args))
