;; * cb-library.el
;; * code
;; ** find or create Inbox.org in dired

(defun cb-find-or-create-inbox-org ()
  "In a dired buffer, find or create Inbox.org"

  ;; detect whether or not search for inbox succeeds
  ;; if fails, create it. 
  (condition-case nil
      (re-search-forward "Inbox\.org$")
    (dired-find-file)
    (error (find-file "Inbox.org"))
    )
  )

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

