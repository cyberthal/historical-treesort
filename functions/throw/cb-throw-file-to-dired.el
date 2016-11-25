;; * cb-throw-file-to-dired.el

;; * code

;; ** subfunctions

;; *** drill down and deliver the file to the inbox

(defun cb-dir-drill-deliver-file ()
  "open dir, open /0-inbox, and paste"

  ;; open target dir and jump to top left for search
  (dired-find-file)
  (goto-char (point-min))

  ;; detect whether or not search for inbox succeeds
  ;; if fails, create it. 
  (condition-case nil
      (re-search-forward "0-inbox$")
    (error (cb-create-0-inbox-dir))
    )

  (dired-find-file) ; open inbox

  ;; move files into inbox
  (other-window 1)
  (dired-do-rename)
  )

;; **** create 0-inbox dir

(defun cb-create-0-inbox-dir ()
  "create 0-inbox dir"

  (dired-create-directory "0-inbox")
  )


;; *** return to original state

(defun cb-dired-upstairs ()
  "open dir, open /0-inbox, and paste"

  (other-window 1)
  (dired-up-directory)
  (dired-up-directory)
  (other-window 1)
  (revert-buffer)
  )

;; ** main defun

(defun cb-throw-file-to-dired ()
  "Throw file(s) or directories to an isearch'd dired target."
  (interactive)

  (cb-find-target-dir)
  (cb-dir-drill-deliver-file)
  (cb-dired-upstairs)
)

(provide 'cb-throw-file-to-dired)
