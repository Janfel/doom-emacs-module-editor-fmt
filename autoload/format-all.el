;;; editor/fmt/autoload/format-all.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +fmt-restrict-buffer-thunk-a (thunk)
  "Redefine `format-all--buffer-thunk' to obey the restriction."
  (save-excursion
    ;; Removed: (save-restriction)
    ;; Removed: (widen)
    (let ((inbuf (current-buffer))
          (input (buffer-string)))
      (with-temp-buffer
        (cl-destructuring-bind (errorp error-output) (funcall thunk input)
          (let* ((no-chg (or errorp
                             (= 0 (let ((case-fold-search nil))
                                    (compare-buffer-substrings
                                     inbuf nil nil nil nil nil)))))
                 (output (cond (errorp nil)
                               (no-chg t)
                               (t (buffer-string)))))
            (list output error-output)))))))

;;;###autoload
(defun +fmt-restrict-run-chain-a (language chain)
  "Redefine `+fmt-restrict-run-chain-a' to obey the restriction."
  (let* ((chain (format-all--normalize-chain chain))
         (chain-tail chain)
         (error-output "")
         (reformatted-by '()))
    (format-all--save-line-number
     (lambda ()
       (cl-loop
        (unless (and chain-tail (= 0 (length error-output)))
          (cl-return))
        (let* ((formatter (car chain-tail))
               (f-name (car formatter))
               (f-args (cdr formatter))
               (f-function (gethash f-name format-all--format-table))
               (f-executable (format-all--formatter-executable f-name)))
          (when format-all-debug
            (message
             "Format-All: Formatting %s as %s using %S%s"
             (buffer-name) language f-name
             (with-temp-buffer
               (dolist (arg f-args) (insert " " (shell-quote-argument arg)))
               (buffer-string))))
          (cl-destructuring-bind (f-output f-error-output)
              (let ((format-all--user-args f-args))
                (funcall f-function f-executable language))
            (let ((f-status :already-formatted))
              (cond ((null f-output)
                     (setq error-output f-error-output)
                     (setq f-status :error))
                    ((not (equal f-output t))
                     (setq reformatted-by
                           (append reformatted-by (list f-name)))
                     (let ((inhibit-read-only t))
                       ;; Removed: (erase-buffer)
                       (delete-region (point-min) (point-max))
                       (insert f-output))
                     (setq f-status :reformatted)))
              (run-hook-with-args 'format-all-after-format-functions
                                  f-name f-status)
              (format-all--update-errors-buffer f-status f-error-output))))
        (setq chain-tail (cdr chain-tail)))
       (message "%s"
                (cond ((not (= 0 (length error-output))) "Formatting error")
                      ((not reformatted-by) "Already formatted")
                      (t "Reformatted!")))))))
