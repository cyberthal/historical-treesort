;; * ts-macros.el
;; * code
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
