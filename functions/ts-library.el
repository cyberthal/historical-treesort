;; * ts-library.el
;; * code
;; ** kill line, with error handling

(defun ts-grab-line ()

;; cut the target line
(goto-char (line-beginning-position))

;; avoids error quit when killing from end of buffer
(condition-case nil
    (kill-line)(kill-line)
    (error nil))

)

;; ** Insert inactive timestamp of current time

(defun ts-org-time-and-date-stamp-inactive () 
  "Insert inactive timestamp of current time"

  ;; Calls org-time-stamp-inactive with universal prefix
  (interactive)
  (org-insert-time-stamp (current-time) t t)
  )

;; ** macro to create a file or switch to the buffer if it's open

;; avoids messiness with creating warnings that underlying file has changed, and/or losing unsaved changes.  

(defmacro ts-create-file-or-switch-to-buffer (var)
  (list 'if
        (list 'f-exists? var)
        (list 'find-file var)
        (list 'progn
              (list 'f-touch var)
              (list 'find-file var)
              )
        )
  )
;; ** provide

(provide 'ts-library)

