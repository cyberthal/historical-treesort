;; * cb-throw-up.el
;; * code
;; ** main defun

(defun cb-throw-up()
"Throw file or text one directory upwards"

(interactive)

(if (eq major-mode 'dired-mode)
    (cb-throw-up-file)
  (cb-throw-up-text)
  )

)

;; ** target = text

(defun cb-throw-up-text ()
"Throw text to ../Inbox.org"

(cb-grab-line)

(f-touch "../Inbox.org")
(find-file "../Inbox.org")

;; if *** offset exists, nil, else create it
(create-inbox-offset)

;; yank to bottom of buffer
(end-of-buffer)
(newline)
(yank)
(save-buffer)

;; return to prior layout
(previous-buffer)
(outline-up-heading 1)
(outline-next-visible-heading 1)

)

;; ** target = file

(defun cb-throw-up-file ()
  "Throw file upwards in the dir tree to the next /0-inbox"

  (if
    ;; return t if immediate parent dir is "0-inbox"
    (equal
     ;; return immediate parent directory
     (file-name-nondirectory
      (directory-file-name default-directory))
     "0-inbox"
     )
    (progn ; then
      (make-directory "../../0-inbox" t)
      (rename-file (dired-get-filename "no-dir") "../../0-inbox/")
      )
  (progn ; else
    (make-directory "../0-inbox" t)
    (rename-file (dired-get-filename "no-dir") "../0-inbox/")
    )
  )
  )

;; ** provide

(provide 'cb-throw-up)
