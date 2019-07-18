;; * ts-throw-up.el
(provide 'ts-throw-up)
;; * code
;; ** main defun

(defun ts-throw-up()
"Throw file or text one directory upwards"

(interactive)

(if (eq major-mode 'dired-mode)
    (ts-throw-up-file)
  (ts-throw-up-text)
  )

)

;; ** target = text

(defun ts-throw-up-text ()
"Throw text to ../Inbox.org"

(ts-grab-line)

;; decide whether to jump up two directories or one
(if
    (ts-parent-equal-inbox)
    (ts-create-file-or-switch-to-buffer "../../Inbox.org")
  (ts-create-file-or-switch-to-buffer "../Inbox.org")
    )

(ts-create-inbox-offset)
(ts-yank-to-bottom-of-buffer)
(ts-prev-buffer-next-heading)
)

;; ** target = file

(defun ts-throw-up-file ()
  "Throw file upwards in the dir tree to the next /0-inbox"

  (if
      (ts-parent-equal-inbox)
    (progn ; then
      (make-directory "../../0-inbox" t)
      (rename-file (dired-get-filename "no-dir") "../../0-inbox/")
      )
  (progn ; else
    (make-directory "../0-inbox" t)
    (rename-file (dired-get-filename "no-dir") "../0-inbox/")
    )
  )
  (revert-buffer)
  )
