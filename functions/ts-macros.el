;; * ts-macros.el
;; * code
;; ** ts-checklist-to-not-done

(defun ts-checklist-to-not-done
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

;; ** ts-pipify-word-list

(defun ts-pipify-word-list
    ()
  "Converts multi-line word list into one line separated by pipes."
  (interactive)

  (end-of-line)
  (insert " | ")
  (delete-char 1)
  (end-of-line)
  )
(global-set-key (kbd "s-n") 'ts-pipify-word-list)

(provide 'ts-macros)

;; ** Zinks.org

(defun ts-dired-zinks
    ()
  "From Dired, creates a 'Zinks' file with anchor org-id link."
  (interactive)

  (find-file "Zinks.org")

  (insert (concat "*** " (file-name-directory buffer-file-name)))
  (save-buffer)
  (org-id-store-link)
  (forward-line)
  (org-cycle)
  (goto-char (point-max))
  )
