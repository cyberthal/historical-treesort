;; * cb-throw-up.el
;; * code
;; ** main defun

(defun cb-throw-up()
"Throw file or text to ../"

(interactive)

;; ensure destination exists
(make-directory "../../0-inbox" t)
(f-touch "../../0-inbox/Inbox.org")

(if (eq major-mode 'dired-mode)
    (cb-throw-up-file)
  (cb-throw-up-text)
  )

)

;; ** target = text

(defun cb-throw-up-text ()
"Throw text to ../0-inbox/Inbox.org"

(cb-grab-line)

(find-file "../../0-inbox/Inbox.org")

;; yank to bottom of buffer
(end-of-buffer)
(newline)
(yank)
(save-buffer)

;; return to prior layout
(previous-buffer)
(outline-next-visible-heading 1)

)
;; ** target = file

(defun cb-throw-up-file ()
  "Throw file to ../0-inbox"

  (rename-file (dired-get-filename "no-dir") "../../0-inbox/")

  )

;; ** provide

(provide 'cb-throw-up)
