;; * cb-throw-up.el
(provide 'cb-throw-up)
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

;; decide whether to jump up two directories or one
(if
    (cb-parent-equal-inbox)
    (cb-create-file-or-switch-to-buffer "../../Inbox.org")
  (cb-create-file-or-switch-to-buffer "../Inbox.org")
    )

(cb-create-inbox-offset)
(cb-yank-to-bottom-of-buffer)
(cb-prev-buffer-next-heading)
)

;; ** target = file

(defun cb-throw-up-file ()
  "Throw file upwards in the dir tree to the next /0-inbox"

  (if
      (cb-parent-equal-inbox)
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
