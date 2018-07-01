;; * cb-throw-text-to-dired.el
;; * Code 

;; ** Open target file or dir, and yank text

(defun cb-dir-drill-deliver-text ()
  "Open target file or dir, and yank text"

;;open and jump top left
(dired-find-file)
(goto-char (point-min))

;; detect whether file or directory
(if (eq major-mode 'dired-mode)
    (cb-throw-text-to-dir)
  (cb-throw-text-to-file)
  )
)

;; *** final target is a file

(defun cb-throw-text-to-file ()
"Last mile delivery of text to target file."

(goto-char (point-min))

;; put point either before first top-level heading or at end of buffer
(condition-case nil
(progn 
  (re-search-forward "^* ") ; deliver right before first heading
  (beginning-of-line)
  (open-line 1)
)
(error (progn
          (goto-char (point-max))
          (newline)
          )) ; end error
) ; end condition case 

;; yank and save
(yank)
(save-buffer)

;; Sloppy imperative programming method of returning to prior layout.
(previous-buffer)
(other-window 1)

)

;; *** final target is a dir

(defun cb-throw-text-to-dir ()
"Last mile delivery of text to Inbox.org"

(cb-find-or-create-inbox-org)

;; yank to bottom of buffer
(goto-char (point-max))
(newline)
(yank)
(save-buffer)

;; return to original state
;; Sloppy imperative programming method of returning to prior layout.
(dired (file-name-directory buffer-file-name))
(dired-up-directory)
(other-window 1)

)
;; ** main defun

(defun cb-throw-text-to-dired ()

   ;; test validity of user isearch input
  (cb-find-target-dir)
  (other-window 1)

  (cb-grab-line)
  (other-window 1)
  (cb-dir-drill-deliver-text)

  ;; goto next heading
  (outline-next-visible-heading 1)

) ; end defun

(provide 'cb-throw-text-to-dired)

