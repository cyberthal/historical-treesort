;; * cb-library.el
;; * code
;; ** cb-Grab-Line

(defun cb-Grab-Line ()

;; cut the target heading
(goto-char (line-beginning-position))

;; avoids error quit when killing from end of buffer
(condition-case nil
    (kill-line)(kill-line)
    (error nil))

)

(provide 'cb-library)

;; **cb-org-time-and-date-stamp-inactive

(defun cb-org-time-and-date-stamp-inactive () 
  "Calls org-time-stamp-inactive with universal prefix" 
  (interactive)
  (org-insert-time-stamp (current-time) t t)
  )
