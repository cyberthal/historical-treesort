;; * cb-throw-library.el

;; * code

;; ** Find the target directory in other window

(defun cb-find-target-dir ()
  "Finds targeted dir in other window"

  (other-window 1)
  (goto-char (point-min))
  (forward-line)

  ;; find target dir using manual isearch interface
  (isearch-forward)

  ) ; end defun

(provide 'cb-throw-library)

;; ** if *** offset exists, nil, else create it

(defun create-inbox-offset ()
  "Creates *** offset at top of Inbox.org"
  (interactive)

  (beginning-of-buffer)

  (if

      ;; return nil if buffer is new and empty
      (thing-at-point 'line)

      ()

    (progn
      (insert "*** offset")
      (newline)
      )
    )

  )

;; ** find or create Inbox.org in dired

(defun cb-find-or-create-inbox-org ()
  "In a dired buffer, find or create Inbox.org"

  ;; detect whether or not search for inbox succeeds
  ;; if fails, create it. 
  (condition-case nil
      (progn 
        (re-search-forward "Inbox\.org$")
        (dired-find-file)
        )
    (error 
     (progn 
       (find-file "Inbox.org")
       (create-inbox-offset)
       )
     )
    )
  )
