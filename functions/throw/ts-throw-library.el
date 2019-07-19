;; * ts-throw-library.el
(provide 'ts-throw-library)

;; * code

;; ** Find the target directory in other window

(defun ts-find-target-dir ()
  "Finds targeted dir in other window"

  (other-window 1)
  (goto-char (point-min))
  (forward-line)

  ;; find target dir using manual isearch interface
  (isearch-forward)

  ) ; end defun

;; ** Inbox.org creation
;; *** for Inbox.org, if *** offset exists, nil, else create it

(defun ts-create-inbox-offset ()
  "Creates *** offset at top of Inbox.org"
  (interactive)

  (widen)
  (goto-char (point-min))

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

;; *** find or create Inbox.org in dired

(defun ts-find-or-create-inbox-org ()
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
       (ts-create-inbox-offset)
       )
     )
    )
  )

;; ** yank to bottom of buffer

(defun ts-yank-to-bottom-of-buffer ()
  "Yank to bottom of buffer"
  (end-of-buffer)
  (newline)
  (yank)
  (save-buffer)
  )

;; ** return to prior layout

(defun ts-prev-buffer-next-heading ()
"Return to prior buffer and move point to next heading."
(previous-buffer)
(outline-next-visible-heading 1)
)
;; ** return t if immediate parent dir is "0-inbox"

(defun ts-parent-equal-inbox ()
  "return t if immediate parent dir is 0-inbox"

  (equal
   ;; return immediate parent directory
   (file-name-nondirectory
    (directory-file-name default-directory))
   "0-inbox"
   )
  )
