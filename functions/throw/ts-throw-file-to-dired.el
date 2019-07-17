;; * ts-throw-file-to-dired.el

;; * code

;; ** subfunctions

;; *** drill down and deliver the file to the inbox

(defun ts-dir-drill-deliver-file ()
  "open dir, open /0-inbox, and paste"

  ;; open target dir and jump to top left for search
  (dired-find-file)
  (goto-char (point-min))

  ;; detect whether or not search for inbox succeeds
  ;; if fails, create it.
  (condition-case nil
      (re-search-forward "0-inbox$")
    (error (ts-create-0-inbox-dir))
    )

  (dired-find-file) ; open inbox

  ;; move files into inbox
  (other-window 1)
  (dired-do-rename)
  )

;; **** create 0-inbox dir

(defun ts-create-0-inbox-dir ()
  "create 0-inbox dir"

  (dired-create-directory "0-inbox")
  )


;; *** return to original state

(defun ts-dired-upstairs ()
  "open dir, open /0-inbox, and paste"

  (other-window 1)
  (dired-up-directory)
  (dired-up-directory)
  (other-window 1)
  (revert-buffer)
  )

;; ** main defun

(defun ts-throw-file-to-dired ()
  "Throw file(s) or directories to an isearch'd dired target."
  (interactive)

  (ts-find-target-dir)
  (ts-dir-drill-deliver-file)
  (ts-dired-upstairs)
)

(provide 'ts-throw-file-to-dired)
