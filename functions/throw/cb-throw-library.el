;; * cb-throw-library.el

;; * code

;; ** Find the target directory in other window

(defun cb-find-target-dir ()
  "Finds targeted dir in other window"

  (other-window 1)
  (goto-char (point-min))
  (forward-line)

  ;; find target dir using manual isearch interface
  (isearch-forward)

  ) ; end defun


