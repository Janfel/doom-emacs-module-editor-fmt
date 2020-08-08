;;; editor/fmt/redefinitions.el -*- lexical-binding: t; -*-


(defadvice! +format-all--buffer-thunk-restriction-a (thunk)
  "Redefine `format-all--buffer-thunk' to obey the restriction."
  :override #'format-all--buffer-thunk
  (save-excursion
    (save-restriction
      ;; (widen)
      (let ((inbuf (current-buffer))
            (input (buffer-string)))
        (with-temp-buffer
          (cl-destructuring-bind (errorp errput) (funcall thunk input)
            (let* ((no-chg (or errorp
                               (= 0 (let ((case-fold-search nil))
                                      (compare-buffer-substrings
                                       inbuf nil nil nil nil nil)))))
                   (output (cond (errorp nil)
                                 (no-chg t)
                                 (t (buffer-string)))))
              (list output errput))))))))

(defadvice! +format-all-buffer--with-restriction-a (formatter language)
  "redefine `format-all-buffer--with' to obey the restriction."
  :override #'format-all-buffer--with
  (when format-all-debug
    (message "format-all: formatting %s using %s"
             (buffer-name) (list formatter language)))
  (let ((f-function (gethash formatter format-all--format-table))
        (executable (format-all--formatter-executable formatter)))
    (cl-destructuring-bind (output errput)
        (funcall f-function executable language)
      (let ((status (cond ((null output) :error)
                          ((equal t output) :already-formatted)
                          (t :reformatted))))
        (when (equal :reformatted status)
          ;; (widen)
          (format-all--save-line-number
           (lambda ()
             ;; (erase-buffer)
             (delete-region (point-min) (point-max))
             (insert output))))
        (format-all--show-or-hide-errors errput)
        (run-hook-with-args 'format-all-after-format-functions
                            formatter status)
        (message (cl-ecase status
                   (:error "formatting error")
                   (:already-formatted "already formatted")
                   (:reformatted "reformatted!")))))))
