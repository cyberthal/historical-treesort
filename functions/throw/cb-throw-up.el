;; * cb-throw-up.el
;; * code
;; ** main defun

(defun cb-throw-up()
"Throw file or text to ../"

(interactive)

;; ensure destination exists
(make-directory "../../0-inbox" t)

(if (eq major-mode 'dired-mode)
    (cb-throw-up-file)
  (cb-throw-up-text)
  )

)

;; ** target = text

(defun cb-throw-up-text ()
"Throw text to ../0-inbox/Inbox.org"

(cb-grab-line)

(f-touch "../../0-inbox/Inbox.org")
(find-file "../../0-inbox/Inbox.org")

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

;; ** target = file

(defun cb-throw-up-file ()
  "Throw file to ../0-inbox"

  (rename-file (dired-get-filename "no-dir") "../../0-inbox/")

  )

;; ** provide

(provide 'cb-throw-up)
