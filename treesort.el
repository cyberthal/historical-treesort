;;; treesort.el --- move text/files around directory tree

;; Copyright (C) 2019  Leo Buchignani III

;; Author: Leo Buchignani III <texas.cyberthal@gmail.com>
;; Keywords: outlines, files, convenience
;; Package-Requires:
;; URL:
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Treesort moves text and files through a directory tree.

;; Treesort's main command is te-throw. It moves text or files from the current window to a target in the next window. A second function, te-throw-up, moves text or files up one directory level. You can throw directories the same as files.

;; When you throw a file to a directory, te-throw creates a child directory <target-directory>/0-Inbox/ and puts the file there. This makes it easy to remember which files are new arrivals.

;; When you throw text to a directory, te-throw creates a file Inbox.org. Lots of these files get created during a filing session. You can quickly delete them with te-delete-this-buffer-and-file.

;; Treesort can rapidly change the directory tree structure of your notes. It helps to have some links that won't break when paths change. Use te-dired-zinks to create a file with an org-id link in it.

;; te-throw can throw text into existing files or outlines. You can duplicate a heading to another window with te-duplicate-heading-to-other-window.

;; When you throw text to an outline, te-throw believes that the parent heading is at the top of the visible region. It will only throw to direct children of the parent. You should narrow appropriately before throwing.

;; When you throw text to a file, te-throw puts the text at the bottom. EXCEPT when the file already has a level-1 heading. Then te-throw assumes this is a polished document, not an inbox file. te-throw worries you will forget about text appended to polished documents. So it prepends the text before the level-1 headline, where it will stick out like a sore thumb.

;; te-throw assumes that most headings you file will have four or more stars. Why? Imagine you are throwing headings to an outline. The level-1 heading is the document title. The level-2 headings are categories. The level-3 headings are subcategories. The level-4 headings are topics. Outlines become unwieldy when they get too deep, at which point it's better to create more files and directories to spread the load.

;; te-throw only imposes this opinion on you in one way: it creates Inbox.org files with a "*** offset" at the top. You can still file level-5 headings, but they might "vanish" if you accidentally file a level-4 heading that folds appended level-5 headings beneath it. You can also file level-3 headings, although they won't be children of the "offset" heading, and might unexpectedly fold appended level-4 headings. I recommend that you convert headings to level 4 for transport, and then resize them at their destination.

;; The last text thrown is saved in the variable te-object-text until the Emacs session ends. Text is not saved to the kill ring.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file: treesort.el

;; (require 'treesort)

;;;; Usage

;; Run one of these commands:

;; `te-throw' throw text/files to the next window
;; `te-throw-up' throw text/files one directory up
;; `te-delete-this-buffer-and-file' self-explanatory
;; `te-store-link-fold-drawer' store an org link and hide the drawer
;; `te-dired-zinks' store an org link in a file, titled with relative path
;; `te-duplicate-heading-to-other-window' self-explanatory

;;;; Tips

;; Use org-id for global link IDs that are not path-dependent.

;; Treesort encourages many org files in deeply nested directories. This can make it challenging to construct an org-agenda files list. See here to load org agenda files recursively: https://stackoverflow.com/questions/17215868/recursively-adding-org-files-in-a-top-level-directory-for-org-agenda-files-take

;; It also helps to have a function that refreshes your org agenda file list, if you've altered paths in that directory.

;; I recommend configuring Dired to sort directories before files. Where possible, capitalize files and directories. This makes it easy to target them with isearch in a few keystrokes. Omit extensions to reduce visual clutter.

;; Treesort filing is fast. Think with your fingers, not your brain. You can always redo it later.

;;; My keybindings

;; By putting the following commands on convenient keys, you can file without thinking about it.

;; (global-set-key (kbd "H-f") 'te-throw)
;; (global-set-key (kbd "H-g") 'te-throw-up)
;; (global-set-key (kbd "C-c k") 'te-delete-this-buffer-and-file)
;; (global-set-key (kbd "C-c l") 'te-store-link-fold-drawer)
;; (global-set-key (kbd "H-a") 'other-window)
;; (global-set-key (kbd "H-w") 'outline-up-heading)
;; (global-set-key (kbd "H-e") 'outline-previous-visible-heading)
;; (global-set-key (kbd "H-r") 'outline-next-visible-heading)
;; (global-set-key (kbd "H-d") 'org-narrow-to-subtree)
;; (global-set-key (kbd "H-s") 'widen)
;; (global-set-key (kbd "H-1") 'spacemacs/toggle-maximize-buffer)
;; (global-set-key (kbd "H-2") 'delete-window)
;; (global-set-key (kbd "H-3") 'split-window-right)
;; (global-set-key (kbd "s-i") 'ido-dired)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License

;;; Code:

;; * treesort.el
;; * offset
;; ** throw DONE

;; *** config DONE

;; **** don't search invisible text in dired DONE

(defun te-dired-dont-search-invisible ()
  (make-local-variable 'search-invisible)
  (setq search-invisible nil)
  )
(add-hook 'dired-mode-hook 'te-dired-dont-search-invisible)
;; *** main defun DONE

(defun te-throw (&optional count)
  "Throw text or dired entry to a target in the next window COUNT times."
  (interactive "p")

  (dotimes (var count)

    (unwind-protect
        (if (eq major-mode 'dired-mode)
            (te-throw-file)
          (te-throw-text))
      (other-window -1) ; save-selected-window fails for throw-text
      )
    )
  )
;; *** flow control dispatcher DONE

;; **** main defun DONE

(defun te-throw-text ()
  "Throw text to either Dired or an outline."

  (select-window (next-window))
  (let ((te-in-dired-p (string-equal major-mode 'dired-mode)))
    (select-window (previous-window))

    (if te-in-dired-p
        (te-throw-text-to-dired)
      (call-interactively 'te-throw-text-to-outline)
      )
    )
  )
;; **** throw file DONE

(defun te-throw-file ()
  "Throw file(s) from one Dired buffer to a searched target in an adjacent Dired buffer."

  (select-window (next-window))
  (te-search-dired-open)
  (mkdir (concat default-directory "0-Inbox/") 1)
  (find-file (concat default-directory "0-Inbox/"))
  (select-window (previous-window))
  (dired-do-rename)

  (select-window (next-window))
  (dired-up-directory) ; restores original dired buffer.
  (dired-up-directory) ; necessary because save-current-buffer won't restore after dired-do-rename.
  (forward-char 1)
  )
;; **** throw text DONE
;; ***** destination = dired DONE

;; ****** main defun DONE

(defun te-throw-text-to-dired ()
  "Throw text to a searched target in an adjacent Dired buffer."

  (select-window (next-window))

  (let ((te-dired-starting-buffer (current-buffer))
        )
  (te-search-dired-open)
  (select-window (previous-window))
  (te-snort-text)
  (select-window (next-window))
  (if buffer-file-name
      (te-insert-text-to-file-blind)
    (te-insert-text-to-directory)
    )
  (switch-to-buffer te-dired-starting-buffer) ; save-current-buffer bugged, must use instead
  (forward-char 1)
  )
  )
;; ****** destination = dir

(defun te-insert-text-to-directory ()
  "Insert te-object-text to Inbox.org."

    (te-create-open-inbox-org)
    (te-insert-to-end-of-buffer)
    (te-text-inserted-to-buffer-path-message)
  )
;; ****** destination = file

(defun te-insert-text-to-file-blind ()
  "Put point either before first level-1 heading or at end of buffer.
Normally one wants to yank to the end of the buffer.
But if it's a polished document rather than an inbox,
then one wants the new text at the top, where its more noticeable.
Function assumes a polished document will have a level-1 near the top."

  (goto-char (point-min))
  (condition-case nil
      (progn
        (re-search-forward "^* ") ; search for a level-1 headline
        (goto-char (point-at-bol))
        (insert te-object-text)
        )
    (error (te-insert-to-end-of-buffer))
    )
  (te-text-inserted-to-buffer-path-message)
  )
;; ***** destination = text
;; ****** main defun DONE

(defun te-throw-text-to-outline (PREFIX)
  "Append text to next window's heading beginning with PREFIX.
Assumes parent heading is at the top of the visible region.

Prompts user for input. Asks for enough letters from the beginning of the target
child heading to distinguish it from the other immediate children of the parent
heading. Searches for a simple string. Takes the first match.

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
             PREFIX)
     )

    (unless (outline-on-heading-p) (user-error "%s" "Search did not find a valid heading"))

    (org-save-outline-visibility 1 ; argument necessary, else heading after body text unfolds body text
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-max))
        (org-N-empty-lines-before-current 1)

        (save-selected-window (select-window (previous-window))
                              (te-snort-text))

        (insert te-object-text)
        )
      )
    )
  )
;; *** throw up DONE
;; **** main defun DONE

(defun te-throw-up (&optional count)
  "Throw file or text one directory upwards, COUNT times."
  (interactive "p")

  (dotimes (var count)

    (if (eq major-mode 'dired-mode)
        (te-throw-up-file)
      (te-throw-up-text))
    )
  )
;; **** jump height DONE

(defun te-jump-destination ()
  "Return a directory either one above current, or two if parent is /0-Inbox."

  (concat default-directory

          ;; "Returns ../ unless parent dir is 0-inbox, then ../../"
          (if (te-parent-dir-inbox-p)
              "../../"
            "../")
          )
  )
;; **** object = text DONE

(defun te-throw-up-text ()
  "Throw text upwards in the directory tree to the next /0-Inbox."

  (let ((te-buffer-home (current-buffer))
        (te-text-object (te-snort-text))
        (default-directory (te-jump-destination))
        )
    (te-create-open-inbox-org)
    (te-insert-text-to-file-blind)
    (switch-to-buffer te-buffer-home) ; because save-current-buffer failed here
    )
  )
;; **** target = file DONE

(defun te-throw-up-file ()
  "Throw file upwards in the directory tree to the next /0-Inbox."

  (let* ((te-jump-destination (te-jump-destination))
         (te-inbox-dir (concat te-jump-destination "0-Inbox/"))
         )
    (if (file-exists-p te-inbox-dir)
        ()
      (mkdir te-inbox-dir)
      )
    (rename-file (dired-get-filename "no-dir") te-inbox-dir)
    (message "File thrown to %s" te-jump-destination)
    )
  (revert-buffer) ; refreshes screen significantly faster than otherwise.
  )
;; *** library DONE
;; **** snort type DONE
;; ***** text mode?

(defun te-snort-text ()
  "If heading or line of text to te-snort-line variable."
  (cond ((eq major-mode 'org-mode) (te-snort-text-org))
        ((-contains-p minor-mode-list 'outshine-mode) (te-snort-text-outshine))
        ((-contains-p minor-mode-list 'outline-minor-mode) (te-snort-text-outline))
        (t (te-snort-line))
        )
  )
;; ***** at heading?

(defun te-snort-text-org ()
     (if (org-at-heading-p) (te-snort-org-heading)
            (te-snort-line))
     )
(defun te-snort-text-outshine ()
     (if (outline-on-heading-p) (te-snort-outshine-heading)
            (te-snort-line))
     )
(defun te-snort-text-outline ()
     (if (outline-on-heading-p) (te-snort-outline-heading)
            (te-snort-line))
     )
;; ***** heading type? DONE

(defun te-snort-org-heading ()
  (save-restriction
    (org-narrow-to-subtree)
    (te-snort-visible)
    )
  )
(defun te-snort-outshine-heading ()
  (save-restriction
    (outshine-narrow-to-subtree)
    (te-snort-visible)
    )
  )
(defun te-snort-outline-heading ()
  (save-restriction
    (org-narrow-to-subtree)
    (te-snort-visible)
    )
  )
;; ***** line

(defun te-snort-line ()
  "Move a line of text to var te-object-text."

  (if (eq (point-min) (point-max))
      (user-error "%s" "Selected line is empty")
    (setq te-object-text
          (concat (delete-and-extract-region (line-beginning-position) (line-end-position))
                  "\n"
                  )
          )
    (te-delete-leftover-empty-line)
    )
  )
;; **** files DONE
;; ***** Find the searched dired entry DONE

(defun te-search-dired-open ()
  "Opens the isearched Dired entry."

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

(defun te-parent-dir-inbox-p ()
  "Return t if parent dir is 0-Inbox."

  (equal
   (file-name-nondirectory (directory-file-name default-directory)) ; returns parent directory
   "0-Inbox")
  )
;; ***** Inbox.org creation DONE
;; ****** Create open Inbox.org DONE

(defun te-create-open-inbox-org ()
  "If Inbox.org doesn't already exist, create it and insert *** offset."

  (let* ((te-inbox-org-path (concat default-directory "Inbox.org"))
         (te-inbox-org-buffer (find-buffer-visiting te-inbox-org-path)))

    (cond (te-inbox-org-buffer (set-buffer te-inbox-org-buffer)) ; select buffer if exists
          ((file-exists-p te-inbox-org-path) (find-file te-inbox-org-path)) ; open file if exists
          ;; else create and open file
          (t (progn (f-touch "Inbox.org")
                    (find-file te-inbox-org-path)
                    (insert "*** offset\n\n")
                    )
             )
          )
    )
  )
;; ** utilities DONE
;; *** te-delete-this-buffer-and-file DONE

(defun te-delete-this-buffer-and-file ()
  "Delete file visited by current buffer and kill buffer."
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

(defun te-store-link-fold-drawer ()
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

(defun te-dired-zinks ()
  "Create Zinks.org and insert an anchor org-id link titled with its path relative to `vc-root-dir' if present, else `user-home-directory'."
  (interactive)

  (let ((zinks-filename (concat default-directory "Zinks.org"))
        )
    (if (file-exists-p zinks-filename)
        (user-error "%s" "Zinks.org already exists")
      (find-file zinks-filename)
      (insert (concat "*** "
                      (expand-file-name (file-name-directory buffer-file-name) (if (vc-root-dir)
                                                                                   (vc-root-dir)
                                                                                 user-home-directory)) ; this might cause an error if outside the user-home-directory and not in a repo. DEFER
                      "\n\n\n"
                      )
              )
      (te-store-link-fold-drawer)
      (goto-char (point-max))
      )
    )
  )
;; *** duplicate heading to other window DONE

(defun te-duplicate-heading-to-other-window ()
  "Insert heading at point to the bottom of the buffer in the next window."
  (interactive)

  (save-restriction
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (org-N-empty-lines-before-current 1)
    (let ((home-buffer (current-buffer))
          )
      (save-selected-window
        (select-window (next-window))
        (goto-char (point-max))
        (org-N-empty-lines-before-current 1)
        (insert-buffer-substring home-buffer)
        )
      )
    )
  )
;; ** library DONE

;; *** snort visible region DONE

(defun te-snort-visible ()
  "Move visible text to the variable te-object-text. Widen. Delete the empty line."

  (goto-char (point-max))
  (org-N-empty-lines-before-current 1)
  (setq te-object-text (delete-and-extract-region (point-min) (point-max)))
  (widen)
  (te-delete-leftover-empty-line)
  )
;; *** safely delete empty line

(defun te-delete-leftover-empty-line ()
  "Deletes empty line at point, if there is one."

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
;; *** insert at bottom of buffer DONE

(defun te-insert-to-end-of-buffer ()
  "Add `te-object-text' text to bottom of target buffer."

  (widen)
  (goto-char (point-max))
  (org-N-empty-lines-before-current 1)
  (insert te-object-text)
  )
;; *** text inserted confirmation message

(defun te-text-inserted-to-buffer-path-message ()
  "Reports the filename the text arrived at, with path relative to vd-root-dir or ~/."

  (message "Inserted text into `%s'" (if (vc-root-dir)
                                         (expand-file-name buffer-file-name (vc-root-dir))
                                       (expand-file-name buffer-file-name user-home-directory)
                                       )
           )
  )
;; ** provide

(provide 'treesort)
;;; treesort.el ends here
