;; * cb-library.el
;; * code
;; ** kill line, with error handling

(defun cb-grab-line ()

;; cut the target line
(goto-char (line-beginning-position))

;; avoids error quit when killing from end of buffer
(condition-case nil
    (kill-line)(kill-line)
    (error nil))

)

;; ** Insert inactive timestamp of current time

(defun cb-org-time-and-date-stamp-inactive () 
  "Insert inactive timestamp of current time"

  ;; Calls org-time-stamp-inactive with universal prefix
  (interactive)
  (org-insert-time-stamp (current-time) t t)
  )

;; ** provide

(provide 'cb-library)

