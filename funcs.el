;; * treesort.el
;; * code
;; ** throw

;; *** config

;; **** don't search invisible text in dired

(add-hook 'dired-mode-hook
          (lambda ()
            (make-local-variable 'search-invisible)
            (setq search-invisible nil)))

;; *** main defun

(defun ts-throw ()
  "Throw text or dired item to a target."

  (interactive)
  (ts-throw-q-is-source-buffer-dired))

;; *** flow control dispatcher

;; **** Is source buffer dired?

(defun ts-throw-q-is-source-buffer-dired ()
  "Determines whether throwing text or files."

(if (eq major-mode 'dired-mode)
    (ts-throw-file)
  (ts-throw-text)))

;; **** throw file

(defun ts-throw-file ()
  "If target buffer not dired, return error."

(other-window 1)

(if (eq major-mode 'dired-mode)
    (progn (other-window 1) (ts-throw-file-to-dired))
  (print "Target buffer type must be dired")))

;; **** throw text


(defun ts-throw-text ()
  "Throwing text. Is target buffer type dired or text?"

  (other-window 1)

  (if (eq major-mode 'dired-mode)
      (progn (other-window 1) (ts-throw-text-to-dired))
    (progn (other-window 1) (call-interactively 'ts-throw-text-to-outline))))

;; *** library
;; **** kill a line

(defun ts-kill-line ()

  (goto-char (line-beginning-position))

  ;; avoids error when killing from end of buffer
  (condition-case nil
      (kill-line)(kill-line)
      (error nil)))

;; **** Find the target directory in other window

(defun ts-find-target-dir ()
  "Finds targeted directory in other window"

  (other-window 1)
  (goto-char (point-min))
  (forward-line)
  (isearch-forward))

;; **** create a file or switch to the buffer if it's open

;; avoids warnings that underlying file has changed, and/or losing unsaved changes.
(defun ts-create-file-or-switch-to-buffer (filename)
  (if (f-exists? filename)
      (find-file filename)
    (progn (f-touch filename) (find-file filename))))

;; **** check whether immediate parent dir is "0-Inbox"

(defun ts-parent-equal-inbox ()
  "return t if immediate parent dir is 0-Inbox"

  (equal
   ;; return immediate parent directory
   (file-name-nondirectory (directory-file-name default-directory))
   "0-Inbox" ))

;; **** Inbox.org creation
;; **** for Inbox.org, if *** offset exists, nil, else create it

(defun ts-create-inbox-offset ()
  "Creates *** offset at top of Inbox.org"
  (interactive)

  (widen)
  (goto-char (point-min))

  ; do nothing unless buffer is new and empty
  (if (thing-at-point 'line)
      ()
    (progn (insert "*** offset") (newline))))

;; **** find or create Inbox.org in dired

(defun ts-find-or-create-inbox-org ()
  "In a dired buffer, find or create Inbox.org"

  (condition-case nil
      (progn (re-search-forward "Inbox\.org$") (dired-find-file))
    (error (progn (find-file "Inbox.org") (ts-create-inbox-offset)))))

;; **** yank to bottom of buffer

(defun ts-yank-to-bottom-of-buffer ()
  "Yank to bottom of buffer"

  (end-of-buffer)
  (newline)
  (yank)
  (save-buffer))

;; **** return to original buffer, advance to next heading

(defun ts-prev-buffer-next-heading ()
  "Return to prior buffer and move point to next heading."

  (previous-buffer)
  (outline-next-visible-heading 1))

;; *** throw up
;; **** main defun

(defun ts-throw-up()
  "Throw file or text one directory upwards"

  (interactive)

  (if (eq major-mode 'dired-mode)
      (ts-throw-up-file)
    (ts-throw-up-text)))

;; **** target = text

(defun ts-throw-up-text ()
  "Throw text to ../Inbox.org"

  (ts-kill-line)

  ;; jump either two directories or one
  (if (ts-parent-equal-inbox)
      (ts-create-file-or-switch-to-buffer "../../Inbox.org")
    (ts-create-file-or-switch-to-buffer "../Inbox.org"))

  (ts-create-inbox-offset)
  (ts-yank-to-bottom-of-buffer)
  (ts-prev-buffer-next-heading))

;; **** target = file

(defun ts-throw-up-file ()
  "Throw file upwards in the dir tree to the next /0-Inbox"

  (if (ts-parent-equal-inbox)
      (progn (make-directory "../../0-Inbox" t)
        (rename-file (dired-get-filename "no-dir") "../../0-Inbox/"))
    (progn (make-directory "../0-Inbox" t)
      (rename-file (dired-get-filename "no-dir") "../0-Inbox/")))
  (revert-buffer) ; refresh dired buffer. necessary?
  )

;; *** throw text to dired
;; **** main defun

(defun ts-throw-text-to-dired ()

  ;; check validity of user's isearch input before making destructive changes
  (ts-find-target-dir)
  (other-window 1)

  (ts-kill-line)
  (other-window 1)
  (ts-dir-drill-deliver-text)
  (outline-next-visible-heading 1))

;; **** drill deliver text

;; ***** main defun

(defun ts-dir-drill-deliver-text ()
  "Open target file or directory, then yank text"

  (dired-find-file)
  (goto-char (point-min))

  (if (eq major-mode 'dired-mode)
      (ts-throw-text-to-dir)
    (ts-throw-text-to-file)))

;; ***** final target is a file

(defun ts-throw-text-to-file ()
  "Yank text to target file."

  (goto-char (point-min))

  ;; Put point either before first top-level heading or at end of buffer.
  ;; Normally one wants to yank to the end of the buffer.
  ;; But if it's a polished document rather than a list of 4*headings,
  ;; then one wants the 4*headings at the top, where they're noticeable.

  (condition-case nil
      (progn
        (re-search-forward "^* ") ; yank right before first heading
        (beginning-of-line) (open-line 1))
    (error (progn (goto-char (point-max)) (newline))))

  (yank)
  (save-buffer)

  ;; Sloppy imperative programming method of returning to the original perspective.
  (previous-buffer)
  (other-window 1))

;; ***** final target is a dir

(defun ts-throw-text-to-dir ()
  "Yank text to Inbox.org"

  (ts-find-or-create-inbox-org)
  (goto-char (point-max))
  (newline)
  (yank)
  (save-buffer)

  ;; Sloppy imperative programming method of returning to original perspective.
  (dired (file-name-directory buffer-file-name))
  (dired-up-directory)
  (other-window 1))

;; *** throw file to dired
;; **** main defun

(defun ts-throw-file-to-dired ()
  "Throw file(s) or directories to an isearch'd dired target."
  (interactive)

  (ts-find-target-dir)
  (ts-dir-drill-deliver-file)
  (ts-dired-upstairs))

;; **** Drill down and deliver the file to the inbox

(defun ts-dir-drill-deliver-file ()
  "Open dir, open /0-Inbox, and yank."

  (dired-find-file) ; Open target dir.
  (goto-char (point-min)) ;  Jump to top left for search.

  ;; Detect whether or not search for Inbox directory succeeds.
  ;; If fails, mkdir.
  (condition-case nil (re-search-forward "0-Inbox$")
    (error ((dired-create-directory "0-Inbox"))))

  (dired-find-file) ; Open Inbox.
  (other-window 1)
  (dired-do-rename) ; Move files into inbox.
  )

;; **** Return to original perspective

(defun ts-dired-upstairs ()
  "Move dired buffer ../../, return point."

  (other-window 1)
  (dired-up-directory)
  (dired-up-directory)
  (other-window 1)
  (revert-buffer))

;; *** throw text to outline
;; **** main defun

(defun ts-throw-text-to-outline (x)
  "Appends current line to the body of a selected child heading.  Assumes parent heading is at the top of the visible region.

Prompts user for input.  Asks for enough letters from the beginning of the target child heading to distinguish it from the other immediate children of the parent heading.  Searches for a simple string.  Takes the first match.  Does not take any other arguments.

If no match found, fails with an error, and does not kill the line."

  (interactive "sEnter target heading's unique prefix: ")

  ;; fail with an error if user passes a bad heading prefix BEFORE deleting the line to be thrown.  To avoid data loss
  (save-excursion
    (ts-goto-target-heading))
  (other-window 1)

  ;; safe to proceed.  commence throwing the line.

  ;; cut the object line
  (beginning-of-line)
  (kill-line 1)

  (ts-goto-target-heading)

  ;; go to end of target heading, add a blank line, and yank.
  (org-narrow-to-subtree)
  (goto-char (point-max))
  (newline)
  (yank)
  (goto-char (point-max)) ; prevents edge case where yank into empty category causes failure to restore perspective

  ;; restore original perspective
  (goto-char (point-min))
  (widen)
  (outline-up-heading 1)
  (org-narrow-to-subtree)
  (other-window 1))

;; **** goto target heading

;; ***** main defun

(defun ts-goto-target-heading ()
  "Finds a heading in the other window."

  (search-forward
   (concat (ts-print-starry-string)
           (message "%s" x) ; user-entered text
           )))

;; ***** print starry string

;; ****** main defun

(defun ts-print-starry-string ()
  "Print the n* prefix of the target heading
for the search string."

  ;; side effect: switches to other window, top heading
  (concat "\n" ; newline avoids grabbing subheading matches
   (make-string (+ (ts-top-heading-stars) 1) ?*) ; makes n *'s
   " "))

;; ****** target max heading level

(defun ts-top-heading-stars ()
  "Returns target window's top outline heading level.
Bounces point to target top visible heading & counts *'s."

  (other-window 1)
  (goto-char (point-min))
  (skip-chars-forward "*"))

;; ** minor utilities
;; *** C-c k QUICK KILL

(defun ts-delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)

  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))

    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (if (buffer-narrowed-p)
          (error "Buffer is narrowed!")
        (when (yes-or-no-p "Are you sure you want to remove this file? ")
          (delete-file filename)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename))))))

;; *** pipify word list

(defun ts-pipify-word-list
    ()
  "Converts multi-line word list into one line separated by pipes."
  (interactive)

  (end-of-line)
  (insert " | ")
  (delete-char 1)
  (end-of-line))

;; *** create Zinks.org

(defun ts-dired-zinks
    ()
  "From Dired, creates a 'Zinks' file with anchor org-id link."
  (interactive)

  (find-file "Zinks.org")

  (insert (concat "*** " (file-name-directory buffer-file-name)))
  (save-buffer)
  (org-id-store-link)

  ;; superfluous if I configure org-mode to create these drawers closed
  (forward-line)
  (org-cycle)

  (goto-char (point-max)))

;; *** decompose a heading, after saving a mummy of it

(defun ts-decomposing-mummy ()
  "From a single-window frame in org-mode,
do setup to decompose a heading."
  (interactive)

  (mwim-beginning-of-code-or-line)
  (kill-new "")
  (kill-whole-line)
  (org-yank)
  (org-yank)
  (org-previous-visible-heading 2)
  (org-cycle)
  (mwim-end-of-line-or-code)
  (insert " | MUMMY")
  (org-cycle)
  (org-next-visible-heading 2)
  (org-ctrl-c-ret)

  (let ((position (point)))
    (clone-indirect-buffer-other-window "'Temporary" t)
    (goto-char position))
  (org-narrow-to-subtree)
  (other-window 1)
  (org-previous-visible-heading 1)
  (org-cycle)
  (org-narrow-to-subtree))

;; *** Store link and hide the PROPERTIES drawer

(defun ts-store-link-hide-drawer ()
    (interactive)

(save-excursion
  (org-store-link nil t)
  (org-narrow-to-subtree)
  (outline-previous-visible-heading 1)
  (next-line)
  (org-cycle)))

;; ** provide

(provide 'treesort)
