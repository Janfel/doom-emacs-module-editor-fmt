;;; autoload/fmt-luaformatter.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

;;; LuaFormatter - Code formatter for Lua
;;; https://github.com/Koihik/LuaFormatter

(defvar +fmt-luaformatter-program "lua-format"
  "The executable for `+fmt-luaformatter-format-region'.")

(defvar +fmt-luaformatter-config-file nil
  "The configuration file for `+fmt-luaformatter-format-region'.")

(defun +fmt-luaformatter-compute-args ()
  "Compute arguments for `+fmt-luaformatter-format-region'."
  (let ((indent      (if indent-tabs-mode 1 standard-indent))
        (cont-indent (if indent-tabs-mode 1 standard-indent)))
    (nconc
     (when (and +fmt-luaformatter-config-file
                (file-readable-p +fmt-luaformatter-config-file))
       (list "--config" +fmt-luaformatter-config-file))
     (list
      (if indent-tabs-mode "--use-tab" "--no-use-tab")
      "--indent-width"               (number-to-string indent)
      "--continuation-indent-width"  (number-to-string cont-indent)
      "--tab-width"                  (number-to-string tab-width)))))

;;;###autoload (autoload '+fmt-luaformatter-format-buffer "autoload/fmt-luaformatter" nil t)
;;;###autoload (autoload '+fmt-luaformatter-format-region "autoload/fmt-luaformatter" nil t)
(+fmt-define +fmt-luaformatter
  :program +fmt-luaformatter-program
  :args (+fmt-luaformatter-compute-args))
