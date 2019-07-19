;; * treesort.el
;; * offset
;; ** throw

;; *** main defun

(defun ts-throw ()
  "Throw text or dired item to a target."

  (interactive)
  (ts-throw-q-is-source-buffer-dired)
  )

;; *** flow control dispatcher

;; **** Is source buffer dired?

(defun ts-throw-q-is-source-buffer-dired ()
  "Determines whether throwing text or files."

(if (eq major-mode 'dired-mode)
    (ts-throw-file)
  (ts-throw-text)
  )
)

;; **** throw file

(defun ts-throw-file ()
  "Exists for code legibility."
(ts-throw-q-is-file-target-buffer-dired)
)

;; ***** Throwing file.  Is target buffer dired?

(defun ts-throw-q-is-file-target-buffer-dired ()
  "If target buffer not dired, return error."

(other-window 1)
(if (eq major-mode 'dired-mode)

    ;; then
    (progn
      (other-window 1)
      (ts-throw-file-to-dired)
      )

  ;; else
  (print "Target buffer type must be dired")
  )

)

;; **** throw text

(defun ts-throw-text ()
  "Code legibility transformation."
  (ts-throw-q-is-text-target-buffer-dired)
)

;; ***** Throwing text. Is target buffer type dired? 

(defun ts-throw-q-is-text-target-buffer-dired ()
  "Throwing text. Is target buffer type dired?"

(other-window 1)
(if (eq major-mode 'dired-mode)

;; then
(progn
    (other-window 1)
    (ts-throw-text-to-dired)
    )

;; else
(progn
(other-window 1)
  (call-interactively
   'ts-throw-text-to-outline)
  )

  ) ; end if
) ; end defun

;; *** library
;; **** kill a line

(defun ts-grab-line ()

  ;; cut the target line
  (goto-char (line-beginning-position))

  ;; avoids error quit when killing from end of buffer
  (condition-case nil
      (kill-line)(kill-line)
      (error nil))

  )

;; **** don't search invisible text in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (make-local-variable 'search-invisible)
            (setq search-invisible nil)))

;; **** Find the target directory in other window

(defun ts-find-target-dir ()
  "Finds targeted dir in other window"

  (other-window 1)
  (goto-char (point-min))
  (forward-line)

  ;; find target dir using manual isearch interface
  (isearch-forward)

  ) ; end defun

;; **** create a file or switch to the buffer if it's open

;; avoids messiness with creating warnings that underlying file has changed, and/or losing unsaved changes.  

(defmacro ts-create-file-or-switch-to-buffer (var)
  (list 'if
        (list 'f-exists? var)
        (list 'find-file var)
        (list 'progn
              (list 'f-touch var)
              (list 'find-file var)
              )
        )
  )

;; **** check whether immediate parent dir is "0-inbox"

(defun ts-parent-equal-inbox ()
  "return t if immediate parent dir is 0-inbox"

  (equal
   ;; return immediate parent directory
   (file-name-nondirectory
    (directory-file-name default-directory))
   "0-inbox"
   )
  )

;; **** Inbox.org creation
;; **** for Inbox.org, if *** offset exists, nil, else create it

(defun ts-create-inbox-offset ()
  "Creates *** offset at top of Inbox.org"
  (interactive)

  (widen)
  (goto-char (point-min))

  (if

      ;; return nil if buffer is new and empty
      (thing-at-point 'line)

      ()

    (progn
      (insert "*** offset")
      (newline)
      )
    )

  )

;; **** find or create Inbox.org in dired

(defun ts-find-or-create-inbox-org ()
  "In a dired buffer, find or create Inbox.org"

  ;; detect whether or not search for inbox succeeds
  ;; if fails, create it. 
  (condition-case nil
      (progn 
        (re-search-forward "Inbox\.org$")
        (dired-find-file)
        )
    (error 
     (progn 
       (find-file "Inbox.org")
       (ts-create-inbox-offset)
       )
     )
    )
  )

;; **** yank to bottom of buffer

(defun ts-yank-to-bottom-of-buffer ()
  "Yank to bottom of buffer"
  (end-of-buffer)
  (newline)
  (yank)
  (save-buffer)
  )

;; **** return to prior layout

(defun ts-prev-buffer-next-heading ()
"Return to prior buffer and move point to next heading."
(previous-buffer)
(outline-next-visible-heading 1)
)

;; *** throw up
;; **** main defun

(defun ts-throw-up()
  "Throw file or text one directory upwards"

  (interactive)

  (if (eq major-mode 'dired-mode)
      (ts-throw-up-file)
    (ts-throw-up-text)
    )

  )

;; **** target = text

(defun ts-throw-up-text ()
  "Throw text to ../Inbox.org"

  (ts-grab-line)

  ;; decide whether to jump up two directories or one
  (if
      (ts-parent-equal-inbox)
      (ts-create-file-or-switch-to-buffer "../../Inbox.org")
    (ts-create-file-or-switch-to-buffer "../Inbox.org")
    )

  (ts-create-inbox-offset)
  (ts-yank-to-bottom-of-buffer)
  (ts-prev-buffer-next-heading)
  )

;; **** target = file

(defun ts-throw-up-file ()
  "Throw file upwards in the dir tree to the next /0-inbox"

  (if
      (ts-parent-equal-inbox)
      (progn ; then
        (make-directory "../../0-inbox" t)
        (rename-file (dired-get-filename "no-dir") "../../0-inbox/")
        )
    (progn ; else
      (make-directory "../0-inbox" t)
      (rename-file (dired-get-filename "no-dir") "../0-inbox/")
      )
    )
  (revert-buffer)
  )

;; *** throw text to dired
;; **** main defun

(defun ts-throw-text-to-dired ()

  ;; test validity of user isearch input
  (ts-find-target-dir)
  (other-window 1)

  (ts-grab-line)
  (other-window 1)
  (ts-dir-drill-deliver-text)

  ;; goto next heading
  (outline-next-visible-heading 1)

  ) ; end defun

;; **** drill deliver text

;; ***** main defun

(defun ts-dir-drill-deliver-text ()
  "Open target file or dir, and yank text"

;;open and jump top left
(dired-find-file)
(goto-char (point-min))

;; detect whether file or directory
(if (eq major-mode 'dired-mode)
    (ts-throw-text-to-dir)
  (ts-throw-text-to-file)
  )
)

;; ***** final target is a file

(defun ts-throw-text-to-file ()
"Last mile delivery of text to target file."

(goto-char (point-min))

;; put point either before first top-level heading or at end of buffer
(condition-case nil
(progn 
  (re-search-forward "^* ") ; deliver right before first heading
  (beginning-of-line)
  (open-line 1)
)
(error (progn
          (goto-char (point-max))
          (newline)
          )) ; end error
) ; end condition case 

;; yank and save
(yank)
(save-buffer)

;; Sloppy imperative programming method of returning to prior layout.
(previous-buffer)
(other-window 1)

)

;; ***** final target is a dir

(defun ts-throw-text-to-dir ()
"Last mile delivery of text to Inbox.org"

(ts-find-or-create-inbox-org)

;; yank to bottom of buffer
(goto-char (point-max))
(newline)
(yank)
(save-buffer)

;; return to original state
;; Sloppy imperative programming method of returning to prior layout.
(dired (file-name-directory buffer-file-name))
(dired-up-directory)
(other-window 1)

)

;; *** throw file to dired
;; **** main defun

(defun ts-throw-file-to-dired ()
  "Throw file(s) or directories to an isearch'd dired target."
  (interactive)

  (ts-find-target-dir)
  (ts-dir-drill-deliver-file)
  (ts-dired-upstairs)
  )

;; **** drill down and deliver the file to the inbox

;; ***** main defun

(defun ts-dir-drill-deliver-file ()
  "open dir, open /0-inbox, and paste"

  ;; open target dir and jump to top left for search
  (dired-find-file)
  (goto-char (point-min))

  ;; detect whether or not search for inbox succeeds
  ;; if fails, create it.
  (condition-case nil
      (re-search-forward "0-inbox$")
    (error (ts-create-0-inbox-dir))
    )

  (dired-find-file) ; open inbox

  ;; move files into inbox
  (other-window 1)
  (dired-do-rename)
  )

;; ***** create 0-Inbox dir

(defun ts-create-0-inbox-dir ()
  "create 0-inbox dir"

  (dired-create-directory "0-inbox")
  )

;; **** return to original state

(defun ts-dired-upstairs ()
  "open dir, open /0-inbox, and paste"

  (other-window 1)
  (dired-up-directory)
  (dired-up-directory)
  (other-window 1)
  (revert-buffer)
  )

;; *** throw text to outline
;; **** main defun

(defun ts-throw-text-to-outline (x)
  "Appends current line to the body of a selected child heading.  Assumes parent heading is at the top of the visible region.

Prompts user for input.  Asks for enough letters from the beginning of the target child heading to distinguish it from the other immediate children of the parent heading.  Searches for a simple string.  Takes the first match.  Does not take any other arguments.

If no match found, fails with an error, and does not kill the line."

  (interactive "sEnter target heading's unique prefix: ")

  ;; fail with an error if user passes a bad heading prefix BEFORE deleting the line to be thrown.  Avoids data loss

(save-excursion
  (ts-goto-target-heading)
  )
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
    (goto-char (point-max)) ; prevents edge case where yank into empty category fails to restore correct visibility in target window

   ;; restore original visibility of target window
(goto-char (point-min))
(widen)
; (org-global-cycle)
    (outline-up-heading 1)
    (org-narrow-to-subtree)
  
  ;; return to source window
    (other-window 1)
    (outline-up-heading 1)
  (outline-next-visible-heading 1)
  
)   ; end of Throw defun

;; **** goto target heading

;; ***** main defun

(defun ts-goto-target-heading ()
  "Goes to targeted heading"

;; net effect - switches to other window, then jumps to targeted heading

    ;; find target heading
    (search-forward
     (concat
      (ts-print-starry-string)
      (message "%s" x) ; appends user-entered prefix to search string
      )))

;; ***** print starry string

;; *****  main defun

(defun ts-print-starry-string ()
  "Print starry string prefix of target heading 
for the search string."

  ;; side effect: switches to other window, top heading

  (concat
   "\n" ;; newline to avoid grabbing subheading matches
   (make-string (+ (ts-target-max-heading-level) 1) ?*) ; makes n *'s
   " "))

;; *****  target max heading level

(defun ts-target-max-heading-level ()
  "Returns target window's max outline heading level.
Bounces point to target top visible heading & counts stars."

  (other-window 1)
  (goto-char (point-min))
  (skip-chars-forward "*") ; move point to the end of the stars, returns number of chars moved forward.
  )

;; ** minor utilities
;; *** C-c k QUICK KILL

;; faster scratch file deletion - defining the function
(defun ts-delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; *** pipify word list

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

;; *** create Zinks.org

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

;; ** provide

(provide 'treesort)
