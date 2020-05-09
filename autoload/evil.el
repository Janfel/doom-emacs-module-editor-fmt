;;; editor/fmt/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+fmt:region "editor/fmt/autoload/evil" nil t)
(evil-define-operator +fmt:region (beg end)
  "Evil ex interface to `+fmt/region'."
  (interactive "<r>")
  (+fmt/region beg end))
