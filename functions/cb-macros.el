;; if buffer is empty, leave this comment or else .emacs throws an error.

(defun cb-checklist-to-not-done
     ()
  "Replaces all checklist X with SPACE"
  (interactive)

  (save-excursion
    (while (search-forward "[X]" nil t)
      (replace-match "[ ]" nil t))
    )
  )

(provide 'cb-macros)
