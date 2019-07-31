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

(defun ts-throw (arg)
  "Throw text or dired item to a target."
  (interactive "p")

  (dotimes (number arg)

    (let ((ts-window-home (selected-window))
          (ts-window-other (next-window))
          )

      ;; Determines whether object is file or text.
      (if (eq major-mode 'dired-mode)
          (ts-throw-file)
        (ts-throw-text))
      )
    )
  )
;; *** flow control dispatcher DONE

;; **** main defun DONE

(defun ts-throw-text ()

  (select-window ts-window-other)
  (if (eq major-mode 'dired-mode)
      (ts-throw-text-to-dired)
    (call-interactively 'ts-throw-text-to-outline))
  )
;; **** throw file DONE

;; the other pieces of this step are in the library.

(defun ts-throw-file ()

  (let* ((ts-buffer-search (buffer-name))
         (ts-searched-file-path (ts-searched-file-path))
         (ts-destination-file-path (concat ts-searched-file-path "/0-Inbox"))
         )

    (mkdir ts-destination-file-path 1)
    (find-file ts-destination-file-path)

    (other-window -1) ; select-window won't work here cuz
    (dired-do-rename) ; this func is interactive with user input

    (select-window ts-window-other 1)
    (switch-to-buffer ts-buffer-search 1)
    (other-window -1)
    )
  )
;; **** throw text DONE
;; ***** destination = dired DONE

;; ****** main defun DONE

(defun ts-throw-text-to-dired ()

  (other-window 1)
  (let* ((ts-searched-file-path (ts-searched-file-path))
        (ts-buffer-search (buffer-name))
        (ts-foo     (other-window -1))
        (ts-text-object (ts-snort-text)))

    (if (file-directory-p ts-searched-file-path)
        (ts-throw-text-to-dir)
      (ts-throw-text-to-buffer))
    )
  (save-buffer)
  )
;; ****** destination = dir

(defun ts-throw-text-to-dir ()
  "Insert text to Inbox.org"

  (select-window ts-window-other)
  (ts-create-inbox-org)
  (ts-insert-to-end-of-buffer)
  (switch-to-buffer ts-buffer-search)
  (select-window ts-window-home)
  )
;; ****** destination = file

(defun ts-throw-text-to-buffer ()
  "Put point either before first top-level heading or at end of buffer.
Normally one wants to yank to the end of the buffer.
But if it's a polished document rather than a list of 4*headings,
then one wants the 4*headings at the top, where they're noticeable.
It is assumed a polished document will have a 1*heading at the top."

  (goto-char (point-min))
  (condition-case nil
      (progn
        (re-search-forward "^* ")
        (goto-char (point-at-bol))
        (insert ts-object-text)
        (save-buffer)
        )
    (error (ts-insert-to-end-of-buffer)))
  )
;; ***** destination = text
;; ****** main defun

(defun ts-throw-text-to-outline (x)
  "Appends current line to the body of a selected child heading.  Assumes parent heading is at the top of the visible region.

Prompts user for input.  Asks for enough letters from the beginning of the target child heading to distinguish it from the other immediate children of the parent heading.  Searches for a simple string.  Takes the first match.

If no match found, fails with an error, and does not kill the line."

  (interactive "sEnter target heading's unique prefix: ")

  ;; interactive window focus is ts-window-other

  ;; fail with an error if user passes a bad heading prefix BEFORE deleting the line to be thrown.  To avoid data loss
  (save-excursion
    (ts-goto-target-heading))
  ;; safe to proceed.  commence throwing the line.

  ;; cut the object line
  (select-window ts-window-home)
  (ts-snort-text)

  (select-window ts-window-other)
  (ts-goto-target-heading)

  ;; go to end of target heading and insert text.
  (org-narrow-to-subtree)
  (goto-char (point-max))
  (ts-empty-line-check)
  (insert ts-object-text)
  (save-buffer)

  ;; restore original perspective.
  (goto-char (point-min))
  (widen)
  (outline-up-heading 1)
  (org-narrow-to-subtree)
  (other-window 1)
  )
;; ****** goto target heading

;; *******  main defun

(defun ts-goto-target-heading ()

  (goto-char (point-min))
  (search-forward
   (concat (ts-print-starry-string)
           (message "%s" x) ; user-entered text
           )
   )
  )
;; *******  print starry string

(defun ts-print-starry-string ()
  "Print the n* prefix of the target heading
for the search string."

  ;; side effect: switches to other window, top heading
  (concat "\n" ; newline avoids grabbing subheading matches
   (make-string (+ (ts-top-heading-stars) 1) ?*) ; makes n *'s
   " ")
  )
;; *******  top heading stars

(defun ts-top-heading-stars ()
  "Returns target window's top outline heading level.
Bounces point to target top visible heading & counts *'s."

  (select-window ts-window-other)
  (goto-char (point-min))
  (skip-chars-forward "*")
  )
;; *** throw up DONE
;; **** main defun DONE

(defun ts-throw-up (arg)
  "Throw file or text one directory upwards"
  (interactive "p")

  (dotimes (number arg)
    (let ((ts-searched-file-path (ts-jump-destination)))

      (if (eq major-mode 'dired-mode)
          (ts-throw-up-file)
        (ts-throw-up-text))
      )
    )
  )
;; **** jump height DONE

(defun ts-jump-destination ()
  "How high should ts-throw-up jump?"

  (let ((ts-home-path default-directory)
        )
    ;; jump either two directories or one
    (if (ts-parent-equal-inbox)
        (setq ts-jump "../../")
      (setq ts-jump "../"))
    (concat ts-home-path ts-jump)
    )
  )
;; **** object = text DONE

(defun ts-throw-up-text ()
  "Throw text to ../Inbox.org."

  (let ((ts-text-object (ts-snort-text)))

    (ts-create-open-inbox-org
     (concat ts-searched-file-path "/Inbox\.org"))
    (ts-throw-text-to-buffer)
    )
  )
;; **** target = file DONE

(defun ts-throw-up-file ()
  "Throw file upwards in the dir tree to the next /0-Inbox"

  (mkdir ts-searched-file-path t)
  (rename-file (dired-get-filename "no-dir") ts-searched-file-path)
  (revert-buffer) ; refresh screen. necessary?
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
;; ***** heading type?

(defun ts-snort-org-heading ()
     ""
     (org-save-outline-visibility 1
                (org-narrow-to-subtree)
              (ts-snort-visible)
              )
     )

(defun ts-snort-outshine-heading ()
  ""
    (outshine-narrow-to-subtree)
    (ts-snort-visible)
    (widen)
  )

(defun ts-snort-outline-heading ()
  ""
    (org-narrow-to-subtree)
    (ts-snort-visible)
    (widen)
  )
;; ***** line

(defun ts-snort-line ()
  ""
  ;; avoids error when snorting line from end of buffer
  (condition-case nil
      (setq ts-object-text
            (delete-and-extract-region (line-beginning-position)
                                       (+ 1 (line-end-position))))
    (error (setq ts-object-text
                 (delete-and-extract-region (line-beginning-position) (buffer-end 1))))
    )
  )
;; **** files DONE
;; ***** Find the searched dired entry DONE

(defun ts-searched-file-path ()
  "Finds the isearched dired entry"

  (other-window 1) ; function will always be called from home window, intended for other window
  (goto-char (point-min))
  (forward-line)
  (isearch-forward)
  (dired-get-filename)
  )

;; ***** check whether immediate parent dir is "0-Inbox" DONE

(defun ts-parent-equal-inbox ()
  "return t if parent dir is 0-Inbox"

  (equal
   (file-name-nondirectory (directory-file-name default-directory)) ; returns parent directory
   "0-Inbox")
  )
;; ***** Inbox.org creation DONE
;; ****** Create Inbox.org DONE

(defun ts-create-inbox-org ()
  "If Inbox.org doesn't already exist, creates it with *** offset."

  (let* ((ts-inbox-file-path (concat ts-searched-file-path "/Inbox\.org"))
        (ts-destination-buffer (find-buffer-visiting ts-inbox-file-path)))

    (cond (ts-destination-buffer (set-buffer ts-destination-buffer)) ; select buffer
          ((file-exists-p ts-inbox-file-path) (find-file ts-inbox-file-path)) ; open file
          ;; or create and open file
          (t (progn (f-touch ts-inbox-file-path)
                    (write-region "*** offset\n" nil ts-inbox-file-path)
                    (find-file ts-inbox-file-path))
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
        (delete-file filename)
        (kill-buffer (current-buffer))
        (message "File '%s' successfully removed" filename)
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
  "Create Zinks.org with an anchor org-id link."
  (interactive)

  (let ((target-filename (concat default-directory "Zinks.org"))
        )
    (if (file-exists-p target-filename)
          (user-error "%s" "Zinks.org already exists")
      (progn (find-file target-filename)
               (insert (concat "*** "
                               (file-name-directory buffer-file-name))) ; would prefer a relative file path instead, e.g. ~/foo DEFER
               (ts-store-link-fold-drawer)
               (ts-ends-n-newlines 2)
               (goto-char (point-max))
               )
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
    (let ((home-buffer (current-buffer)))
      (select-window (next-window)) ; target
      (ts-ends-n-newlines 2)
      (goto-char (point-max))
      (insert-buffer-substring home-buffer))
    (select-window (previous-window)) ; home
    (widen)
    )
  )

;; ** library DONE

;; *** text DONE
;; **** snort visible region DONE

(defun ts-snort-visible ()

  (ts-ends-n-newlines 2)
  (setq ts-object-text (delete-and-extract-region (point-min) (point-max)))
  (widen)
  (unless (eobp)
    (delete-char 1))
  )
;; **** insert at bottom of buffer DONE

(defun ts-insert-to-end-of-buffer ()
  "Adds object text to bottom of target file."

  (widen)
  (goto-char (point-max))
  (ts-empty-line-check)

  (insert ts-object-text)
  (save-buffer)
  )
;; **** if line isn't empty, make newline DONE

(defun ts-empty-line-check ()
  (unless (and (bolp) (eolp))
    (insert "\n"))
)
;; **** visible region ends in two blank lines DONE

(defun ts-ends-n-newlines (&optional arg)
    "Make visible region terminate in one newline, or n newlines with prefix."
  (interactive "p")

  (goto-char (point-max))

  (let* ((arg (if arg arg 1)) ; default newlines = 1
         (number (+ arg (skip-chars-backward "\n")))
        )
    (if (> number 0)
        (insert (make-string number ?\n))
      (delete-char (* -1 number)))
    )
  )
;; *** open destination buffer from filename DONE

(defun ts-open-destination-buffer-from-filename (filename)
  "Open target buffer, if it exists."

  (let ((ts-destination-buffer) (find-buffer-visiting filename))

    (if ts-destination-buffer
        ()
      (find-file filename))
    )
  )
;; ** provide

(provide 'treesort)
