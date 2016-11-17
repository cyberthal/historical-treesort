;; * cb-dthrow.el
;; * Code 

;; ** sub-functions

;; *** Open target dir, Inbox.org, and paste

(defun cb-Dir-Drill-Deliver-Text ()
  "open dir, Inbox.org, and paste"

  (dired-find-file)
  (goto-char (point-min))
  (re-search-forward "Inbox\.org$")
  (dired-find-file)
  (goto-char (point-max))
  (newline)
  (yank)
)
;; ** main defun

(defun cb-dthrow ()
    "Moves line to target dir's Inbox.org

Like cb-throw but with a dired target."

  (interactive)

   ;; test validity of user input
  (cb-Find-Target-Dir)
  (other-window 1)

  (cb-Grab-Line)
  (other-window 1)
  (cb-Dir-Drill-Deliver-Text)
  
  ;; sloppy functional programming method of returning to prior layout.
  (previous-buffer)
  (dired-up-directory)
  (other-window 1)

  ;; goto next heading
  (outline-up-heading 1)
  (outline-next-visible-heading 1)

) ; end defun

(provide 'cb-dthrow)
