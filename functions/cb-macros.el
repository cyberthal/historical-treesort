;; * cb-macros.el
;; * code
;; ** cb-checklist-to-not-done

(defun cb-checklist-to-not-done
     ()
  "Replaces all checklist X with SPACE"
  (interactive)

  (save-excursion
    (while (search-forward "[X]" nil t)
      (replace-match "[ ]" nil t))
    )
  )

;; Org-Mode: "boxes" Advance down org-mode checklist with C-S-n
(fset 'boxes
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("" 0 "%d")) arg)))
;; binding it to a handy key, since as a command it won't macro repeat
(global-set-key (kbd "C-S-n") 'boxes)

;; ** cb-pipify-word-list

(defun cb-pipify-word-list
    ()
  "Converts multi-line word list into one line separated by pipes."
  (interactive)

  (end-of-line)
  (insert " | ")
  (delete-char 1)
  (end-of-line)
  )
(global-set-key (kbd "s-n") 'cb-pipify-word-list)

(provide 'cb-macros)

;; ** Zinks directory

(defun cb-dired-zinks
    ()
  "From Dired, creates a 'Zinks' directory with anchor org-id link."
  (interactive)

  (dired-create-directory "Zinks")
  (dired-find-file)
  (find-file "Inbox.org")
  (insert "**** ")
  (insert (file-name-directory buffer-file-name))
  (search-backward "/1-Human")
  (delete-char 8) ;; remove /1-Human
  (delete-char (- 6 (point)))   ;; remove portion before /1-Human
  (save-buffer)
  ;;  (org-store-link 0) this fails to produce same result as interactive
  ;; return to original position - can't be implemented.
  ;; best to leave it "incomplete" so it can fail gracefully under unexpected conditions
    )
