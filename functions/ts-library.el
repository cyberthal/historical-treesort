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

;; ** C-c k QUICK KILL

;; faster scratch file deletion - defining the function
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; bound function to C-c k
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file) 

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

