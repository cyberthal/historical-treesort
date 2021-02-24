;; ***** create Zinks.org

;;;###autoload
(defun treefactor-org-dired-zinks ()
  "Make Zinks.org.  Insert org-id link.

Link title's path is relative to `vc-root-dir' if present,
else `user-home-directory'."
  (interactive)

  (let ((zinks-filename (concat default-directory "Zinks.org")))
    (if (file-exists-p zinks-filename)
        (user-error "%s" "Zinks.org already exists")
      (find-file zinks-filename)
      (insert (concat "*** "
                      (file-relative-name (file-name-directory buffer-file-name)
                                          (cond ((vc-root-dir) (vc-root-dir))
                                                (user-home-directory user-home-directory) ; Spacemacs variable. If missing, no problem.
                                                ))
                      "\n\n"))
      (treefactor-org-store-link-fold-drawer)
      (save-buffer)
      (goto-char (point-max)))))

