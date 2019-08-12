;; * treesort.el
;; * offset
;; ** throw DONE

;; *** config DONE

;; **** don't search invisible text in dired DONE

(add-hook 'dired-mode-hook
          (lambda ()
            (make-local-variable 'search-invisible)
            (setq search-invisible nil)))
;; *** main defun DONE

(defun ts-throw (&optional count)
  "Throw text or dired entry to a tcountet."
  (interactive "p")

  (dotimes (var count)

    (if (eq major-mode 'dired-mode)
        (ts-throw-file)
      (ts-throw-text))
    (other-window -1) ; save-selected-window fails for throw-text
    )
  )
;; *** flow control dispatcher DONE

;; **** main defun DONE

(defun ts-throw-text ()
  "Throw text to either Dired or an outline."

  (select-window (next-window))
  (let ((ts-in-dired-p (string-equal major-mode 'dired-mode)))
    (select-window (previous-window))

    (if ts-in-dired-p
        (ts-throw-text-to-dired)
      (call-interactively 'ts-throw-text-to-outline)
      )
    )
  )
;; **** throw file DONE

(defun ts-throw-file ()
  "Throw file(s) from one Dired buffer to a searched target in an adjacent Dired buffer."

  (select-window (next-window))
  (ts-search-dired-open)
  (mkdir (concat default-directory "0-Inbox/") 1)
  (find-file (concat default-directory "0-Inbox/"))
  (select-window (previous-window))
  (dired-do-rename)

  (select-window (next-window))
  (dired-up-directory) ; restores original dired buffer.
  (dired-up-directory) ; necessary because save-current-buffer won't restore after dired-do-rename.
  )
;; **** throw text DONE
;; ***** destination = dired DONE

;; ****** main defun DONE

(defun ts-throw-text-to-dired ()
  "Throw text to a searched target in an adjacent Dired buffer."

  (select-window (next-window))

  (let ((ts-dired-starting-buffer (current-buffer)))
  (ts-search-dired-open)
  (select-window (previous-window))
  (ts-snort-text)
  (select-window (next-window))
  (if buffer-file-name
      (ts-insert-text-to-file-blind)
    (ts-insert-text-to-directory)
    )
  (switch-to-buffer ts-dired-starting-buffer) ; save-current-buffer bugged, must use instead
  )
  )
;; ****** destination = dir

(defun ts-insert-text-to-directory ()
  "Insert ts-object-text to Inbox.org"

    (ts-create-open-inbox-org)
    (ts-insert-to-end-of-buffer)
    (ts-text-inserted-to-buffer-path-message)
  )
;; ****** destination = file

(defun ts-insert-text-to-file-blind ()
  "Put point either before first top-level heading or at end of buffer.
Normally one wants to yank to the end of the buffer.
But if it's a polished document rather than a list of 4*headings,
then one wants the 4*headings at the top, where they're noticeable.
It is assumed a polished document will have a 1*heading at the top."

  (goto-char (point-min))
  (condition-case nil
      (progn
        (re-search-forward "^* ") ; search for a top-level headline
        (goto-char (point-at-bol))
        (insert ts-object-text)
        )
    (error (ts-insert-to-end-of-buffer))
    )
  (ts-text-inserted-to-buffer-path-message)
  )
;; ***** destination = text
;; ****** main defun DONE

(defun ts-throw-text-to-outline (arg)
  "Appends current heading or line to the body of a selected child heading.  Assumes parent heading is at the top of the visible region.

Prompts user for input.  Asks for enough letters from the beginning of the target child heading to distinguish it from the other immediate children of the parent heading.  Searches for a simple string.  Takes the first match.

If no match found, fails with an error, and does not delete the line."

  (interactive "sEnter target heading's unique prefix: ")

  (save-selected-window
    (select-window (next-window))

    ;; find searched heading
    (goto-char (point-min))
    (search-forward
     (concat "\n"
             (make-string (+ 1 (skip-chars-forward "*")) ?*)
             " "
             arg)
     )

    (unless (outline-on-heading-p) (user-error "%s" "Search did not find a valid heading"))

    (org-save-outline-visibility 1 ; arg necessary else heading after body text unfolds body text
      (save-restriction
        (org-narrow-to-subtree)
        (ts-ends-n-newlines 2)
        (goto-char (point-max))

        (save-selected-window (select-window (previous-window))
                              (ts-snort-text))

        (insert ts-object-text)
        (ts-ends-n-newlines 1)
        (goto-char (point-min))
        )
      )
    )
  )
;; *** throw up DONE
;; **** main defun DONE

(defun ts-throw-up (&optional count)
  "Throw file or text one directory upwards"
  (interactive "p")

  (dotimes (var count)

    (if (eq major-mode 'dired-mode)
        (ts-throw-up-file)
      (ts-throw-up-text))
    )
  )
;; **** jump height DONE

(defun ts-jump-destination ()
  "Returns a directory either one above current, or two if parent is /0-Inbox"

  (concat default-directory

          ;; "Returns ../ unless parent dir is 0-inbox, then ../../"
          (if (ts-parent-dir-inbox-p)
              "../../"
            "../")
          )
  )
;; **** object = text DONE

(defun ts-throw-up-text ()
  "Throw text to ../Inbox.org."

  (let ((ts-buffer-home (current-buffer))
        (ts-text-object (ts-snort-text))
        (default-directory (ts-jump-destination))
        )
    (ts-create-open-inbox-org)
    (ts-insert-text-to-file-blind)
    (switch-to-buffer ts-buffer-home) ; because save-current-buffer failed here
    )
  )
;; **** target = file DONE

(defun ts-throw-up-file ()
  "Throw file upwards in the dir tree to the next /0-Inbox"

  (let* ((ts-jump-destination (ts-jump-destination))
         (ts-inbox-dir (concat ts-jump-destination "0-Inbox/"))
         )
    (if (file-exists-p ts-inbox-dir)
        ()
      (mkdir ts-inbox-dir)
      )
    (rename-file (dired-get-filename "no-dir") ts-inbox-dir)
    )
  (revert-buffer) ; refreshes screen significantly faster than otherwise.
  )
;; *** library DONE
;; **** snort type DONE
;; ***** text mode?

(defun ts-snort-text ()
  ""
  (cond ((eq major-mode 'org-mode) (ts-snort-text-org))
        ((-contains-p minor-mode-list 'outshine-mode) (ts-snort-text-outshine))
        ((-contains-p minor-mode-list 'outline-minor-mode) (ts-snort-text-outline))
        (t (ts-snort-line))
        )
  )
;; ***** at heading?

(defun ts-snort-text-org ()
     ""
     (if (org-at-heading-p) (ts-snort-org-heading)
            (ts-snort-line))
     )

(defun ts-snort-text-outshine ()
     ""
     (if (outline-on-heading-p) (ts-snort-outshine-heading)
            (ts-snort-line))
     )

(defun ts-snort-text-outline ()
     ""
     (if (outline-on-heading-p) (ts-snort-outline-heading)
            (ts-snort-line))
     )
;; ***** heading type? DONE

(defun ts-snort-org-heading ()

  (save-restriction
    (org-narrow-to-subtree)
    (ts-snort-visible)
    )
  )

(defun ts-snort-outshine-heading ()

  (save-restriction
    (outshine-narrow-to-subtree)
    (ts-snort-visible)
    )
  )

(defun ts-snort-outline-heading ()

  (save-restriction
    (org-narrow-to-subtree)
    (ts-snort-visible)
    )
  )
;; ***** line

(defun ts-snort-line ()
  "Delete a line and save it to ts-object-text"

  (if (eq (point-min) (point-max))
      (user-error "%s" "Selected line is empty")
    (setq ts-object-text
          (concat (delete-and-extract-region (line-beginning-position) (line-end-position))
                  "\n"
                  )
          )
    (ts-delete-leftover-empty-line)
    )
  )
;; **** files DONE
;; ***** Find the searched dired entry DONE

(defun ts-search-dired-open ()
  "Opens the isearched Dired entry"

  (if (string-equal major-mode "dired-mode")
      nil
      (user-error "%s" "Mode must be Dired"))

  (goto-char (point-min))
  (forward-line)
  (dired-hide-details-mode)
  (isearch-forward)
  (dired-find-file)
  )
;; ***** check whether immediate parent dir is "0-Inbox" DONE

(defun ts-parent-dir-inbox-p ()
  "Return t if parent dir is 0-Inbox"

  (equal
   (file-name-nondirectory (directory-file-name default-directory)) ; returns parent directory
   "0-Inbox")
  )
;; ***** Inbox.org creation DONE
;; ****** Create open Inbox.org DONE

(defun ts-create-open-inbox-org ()
  "If Inbox.org doesn't already exist, creates it with *** offset."

  (let* ((ts-inbox-org-path (concat default-directory "Inbox.org"))
         (ts-inbox-org-buffer (find-buffer-visiting ts-inbox-org-path)))

    (cond (ts-inbox-org-buffer (set-buffer ts-inbox-org-buffer)) ; select buffer if exists
          ((file-exists-p ts-inbox-org-path) (find-file ts-inbox-org-path)) ; open file if exists
          ;; else create and open file
          (t (progn (f-touch "Inbox.org")
                    (find-file ts-inbox-org-path)
                    (insert "*** offset\n\n")
                    )
             )
          )
    )
  )
;; ** utilities DONE
;; *** ts-delete-this-buffer-and-file DONE

(defun ts-delete-this-buffer-and-file ()
  "Delete file visited by current buffer and kills buffer."
  (interactive)

  (let ((filename (buffer-file-name))
        )
    (if (buffer-narrowed-p)
        (user-error "%s" "Buffer is narrowed")
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (kill-buffer (current-buffer))
        (delete-file filename)
        (message "File `%s' successfully removed" filename)
        )
      )
    )
  )
;; *** org links DONE
;; **** Store link and fold the PROPERTIES drawer DONE

(defun ts-store-link-fold-drawer ()
  "Store an org link to a heading, and fold the drawer."
  (interactive)

  (save-excursion
    (save-restriction
      (org-store-link nil t) ; without interactive=t arg, no org link gets created
      (org-narrow-to-subtree)
      (org-previous-visible-heading 1)
      (widen)
      (forward-line)
      )
    (org-cycle-hide-drawers 1)
    )
  )
;; **** create Zinks.org DONE

(defun ts-dired-zinks ()
  "Create Zinks.org and inserts an anchor org-id link titled with its path relative to vc-root-dir if present, else user-home-directory."
  (interactive)

  (let ((zinks-filename (concat default-directory "Zinks.org"))
        )
    (if (file-exists-p zinks-filename)
        (user-error "%s" "Zinks.org already exists")
      (find-file zinks-filename)
      (insert (concat "*** "
                      (expand-file-name (file-name-directory buffer-file-name) (if (vc-root-dir)
                                                                                   (vc-root-dir)
                                                                                 user-home-directory))
                      "\n\n\n"
                      )
              )
      (ts-store-link-fold-drawer)
      (goto-char (point-max))
      )
    )
  )
;; *** duplicate heading to other window DONE

(defun ts-duplicate-heading-to-other-window ()
  "Insert heading at point to the bottom of the other window."
  (interactive)

  (save-restriction
    (org-narrow-to-subtree)
    (ts-ends-n-newlines 1)
    (let ((home-buffer (current-buffer))
          )
      (save-selected-window
        (select-window (next-window))
        (ts-ends-n-newlines 2)
        (goto-char (point-max))
        (insert-buffer-substring home-buffer)
        )
      )
    )
  )
;; ** library DONE

;; *** text DONE
;; **** snort visible region DONE

(defun ts-snort-visible ()
  "Cuts visible to ts-object-text, terminating in an empty line. Widens. Leaves no empty line behind."

  (ts-ends-n-newlines 1)
  (setq ts-object-text (delete-and-extract-region (point-min) (point-max)))
  (widen)
  (ts-delete-leftover-empty-line)
  )
;; **** safely delete empty line

(defun ts-delete-leftover-empty-line ()
  "Deletes empty line at point, if there is one"

  (unless (and (bobp) (eobp))
    (if (bobp)
        (delete-char 1)
      (when
          (org--line-empty-p 1) ; (not my) bug: this wrongly returns nil when point is on an empty line at top of buffer. hence the workaround
        (delete-char -1)
        (unless (eobp) (forward-char 1))
        )
      )
    )
  )
;; **** insert at bottom of buffer DONE

(defun ts-insert-to-end-of-buffer ()
  "Adds object text to bottom of target buffer."

  (widen)
  (ts-ends-n-newlines 2)
  (goto-char (point-max))
  (insert ts-object-text)
  )
;; **** visible region ends in two blank lines DONE

(defun ts-ends-n-newlines (&optional arg)
  "Make visible region terminate in n newlines, default 1."
  (interactive "p")

  (goto-char (point-max))
  (let* ((arg (if arg arg 1)) ; default newlines = 1
         (newlines-to-add (+ arg
                             (skip-chars-backward "\n") ; moves point as well.
                             )
                          )
         )
    (unless (>= arg 0) (user-error "Requested number of newlines `%s' is not greater than or equal to zero" arg))

    (if (> newlines-to-add 0)
        (insert (make-string newlines-to-add ?\n))
      (delete-char (* -1 newlines-to-add)
                   )
      )
    )
  )
;; **** text inserted message

(defun ts-text-inserted-to-buffer-path-message ()
    "Says inserted text into buffer file name, with path relative to vd-root-dir or ~/."

  (message "Inserted text into `%s'" (if (vc-root-dir)
                                         (expand-file-name buffer-file-name (vc-root-dir))
                                       (expand-file-name buffer-file-name user-home-directory)
                                       )
           )
  )
;; ** provide

(provide 'treesort)
