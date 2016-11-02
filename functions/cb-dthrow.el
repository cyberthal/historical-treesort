;; * cb-dthrow.el
;; * Code 

;; ** sub-functions

;; *** Find the target directory in other window

(defun cb-Find-Target-Dir ()
  "Finds targeted dir in other window"

(other-window 1)
(goto-char (point-min))

;; find target dir using manual isearch interface
(isearch-forward)

) ; end defun
;; *** Open target dir, Inbox.org, and paste

(defun cb-Dir-Drill-Deliver ()
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
  (cb-Dir-Drill-Deliver)
  
  ;; sloppy functional programming method of returning to prior layout.
  (previous-buffer)
  (dired-up-directory)
  (other-window 1)

) ; end defun

(provide 'cb-dthrow)
