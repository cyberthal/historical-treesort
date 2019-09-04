;;; trs.el --- Batch refactor and refile text & files -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Leo Buchignani III

;; Author: Leo Buchignani III <texas.cyberthal@gmail.com>
;; Keywords: outlines, files, convenience
;; Package-Requires: ((emacs "24.3") (dash "2.16.0") (f "0.20.0"))
;; URL: https://github.com/cyberthal/treesort
;; Version: 1.0.4

;;; Commentary:

;; Treesort rapidly sorts text and files into outlines and the directory tree.

;; Treesort's main command is trs-refile. It moves text or files from the current
;; window to a target in the next window. A second function, trs-refile-up, moves
;; text or files up one directory level. You can refile directories as well as
;; files.

;; When you refile a file to a directory, trs-refile creates a child directory
;; <target-directory>/0-Inbox/ and puts the file there. This makes it easy to
;; remember which files are new arrivals.

;; When you refile text to a directory, trs-refile creates a file Inbox.org. Many
;; such files are created during a filing session. To quickly delete them, use
;; trs-delete-this-buffer-and-file.

;; Treesort can rapidly change the directory tree structure of your notes. It
;; helps to have some links that won't break when paths change. Use
;; trs-dired-zinks to create a file with an org-id link in it.

;; trs-refile can refile text into existing files or outlines. You can duplicate a
;; heading to another text window with trs-duplicate-heading-to-other-window.

;; When you refile text to an outline, trs-refile believes that the parent heading
;; is at the top of the visible region. It will only refile to direct children of
;; the parent. You should narrow appropriately before refiling.

;; When you refile text to a file, trs-refile puts the text at the bottom. EXCEPT
;; when the file already has a level-1 heading. Then trs-refile assumes this is a
;; polished document, not an inbox file. trs-refile worries you will forget about
;; text appended to polished documents. So it prepends the text before the
;; level-1 headline, where it will stick out like a sore thumb.

;; trs-refile assumes that most headings you file will have four stars. Why?
;; Imagine you are refiling headings to an outline. The level-1 heading is the
;; document title. The level-2 headings are categories. The level-3 headings are
;; sub-categories. The level-4 headings are topics. Outlines become unwieldy
;; when they get too deep, at which point it's better to create more files and
;; directories to spread the load.

;; trs-throw only imposes this opinion on you in one way: it creates Inbox.org
;; files with a "*** offset" at the top. You can still file level-5 headings,
;; but they might "vanish" if you file a level-4 heading that unintentionally folds
;; appended level-5 headings beneath it. You can also file level-3 headings,
;; although they won't be children of the "offset" heading, and might
;; unexpectedly fold appended level-4 headings. I recommend that you convert
;; headings to level 4 for transport, and then resize them at their destination.

;; The last text refiled is saved in the variable trs-object-text until the Emacs
;; session ends. Text is not saved to the kill ring. A message appears in the
;; minibuffer displaying the destination of text and files moved with ts-refile
;; commands.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done. (This package is not yet on MELPA.)

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file: trs.el

;; (require 'trs)

;;;; Usage

;; Run one of these commands:

;; `trs-refile' refile text/files to the next window
;; `trs-refile-up' refile text/files one directory up
;; `trs-delete-this-buffer-and-file' self-explanatory
;; `trs-store-link-fold-drawer' store an org link and hide the drawer
;; `trs-dired-zinks' store an org link in a file, titled with relative path
;; `trs-duplicate-heading-to-other-window' self-explanatory

;;;; Tips

;; Use org-id for global link IDs that are not path-dependent.

;; Treesort encourages many org files in deeply nested directories. This can
;; make it challenging to construct an org-agenda files list. See here to load
;; org agenda files recursively:
;; https://stackoverflow.com/questions/17215868/recursively-adding-org-files-in-a-top-level-directory-for-org-agenda-files-take

;; It also helps to have a function that refreshes your org agenda file list, if
;; you've altered paths in that directory.

;; I recommend configuring Dired to sort directories before files. Where
;; possible, capitalize files and directories. This makes it easy to target them
;; with isearch in a few keystrokes. Omit extensions to reduce visual clutter.

;; Treesort filing is fast. Think with your fingers, not your brain. You can
;; always redo it later.

;;; My keybindings

;; By putting the following commands on convenient keys, you can refile without thinking about it.

;; (global-set-key (kbd "H-f") 'trs-refile)
;; (global-set-key (kbd "H-g") 'trs-refile-up)
;; (global-set-key (kbd "C-c k") 'trs-delete-this-buffer-and-file)
;; (global-set-key (kbd "C-c l") 'trs-store-link-fold-drawer)
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

;;; Comments and whitespace

;; Treesort departs from
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html
;; by placing close-parentheses on lines by themselves where it enhances
;; readability. I find it much easier to keep track of parenthetical nesting
;; with indentation than only by counting and font color. Ruby does something
;; similar by placing end-braces on lines by themselves when they embrace
;; multiple lines.

;; Treesort uses an Outshine-style outline to structure its code. Outline
;; navigation, narrowing and folding negates the disadvantage of using extra
;; lines to show end-parentheses indentation. Outshine supports todo tags. There
;; are two level-1 headings at the beginning of the code due to Outshine's
;; visibility-cycling behavior.

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

;; * trs.el
;; * offset
;; ** config
;; *** require

(require 'org)
(require 'dired)
(require 'dash)
(require 'f)
(require 'avy)

;; *** define vars and funcs

(defvar leo-object-text nil
  "Stores the last text treesort killed or copied.")

(defvar user-home-directory) ; Spacemacs variable

;; *** customization

(defgroup trs nil "Refactor prose and incrementally refile things."
  :group 'convenience
  :group 'files)

;; ** Refile

;; *** config

;; **** define variables and declare functions

(defvar leo-inbox-file-header)

(declare-function outshine-narrow-to-subtree "outshine" ())

;; *** main defun

;;;###autoload
(defun leo-refile (&optional count)
  "Refile text/file to target in next window COUNT times.
Select a line from target list using `isearch' then `avy'."
  (interactive "p")

  (dotimes (_var count)
    (unwind-protect
        (leo-refile-object-mode-check)
      (other-window -1)                ; save-selected-window fails for refile-text
      )
    )
  )

(defun leo-refile-object-mode-check ()
  "Determine correct action based on current window mode.
If in dired, refile files. If not, refile text."

  (if (eq major-mode 'dired-mode)
      (leo-refile-file)
    (leo-refile-text))
  )

;; *** flow control dispatcher

;; **** main defun

(defun leo-refile-text ()
  "Refile text to either Dired or an outline."

  (select-window (next-window))
  (let ((leo-in-dired-p (string-equal major-mode 'dired-mode)))
    (select-window (previous-window))

    (if leo-in-dired-p
        (leo-refile-text-to-dired)
      (call-interactively 'leo-refile-text-to-outline)
      )
    )
  )
;; **** refile file

(defun leo-refile-file ()
  "Refile file(s) from Dired to searched target in next window."

  (select-window (next-window))
  (leo-search-dired-open)
  (mkdir (concat default-directory "0-Inbox/") 1)
  (find-file (concat default-directory "0-Inbox/"))
  (select-window (previous-window))
  (dired-do-rename)

  (select-window (next-window))
  (dired-up-directory)     ; restores original dired buffer.
  (dired-up-directory)     ; necessary because save-current-buffer won't restore
                                        ; after dired-do-rename.
  (forward-char 2)
  )
;; **** refile text
;; ***** destination = dired

;; ****** main defun

(defun leo-refile-text-to-dired ()
  "Refile text to a searched target in an adjacent Dired buffer."

  (select-window (next-window))
  (let ((leo-dired-starting-buffer (current-buffer))
        )
    (leo-search-dired-open)
    (select-window (previous-window))
    (leo-snort-text)
    (select-window (next-window))
    (if buffer-file-name
        (leo-insert-text-to-file-blind)
      (leo-insert-text-to-directory)
      )
    (switch-to-buffer leo-dired-starting-buffer) ; Save-current-buffer bugged,
                                        ; must use instead.
    (forward-char 2)
    )
  )
;; ****** destination = dir

(defun leo-insert-text-to-directory ()
  "Insert `leo-object-text' to Inbox.org."

    (leo-create-open-inbox-file)
    (leo-insert-to-end-of-buffer)
    (leo-text-inserted-to-buffer-path-message)
  )
;; ****** destination = file

(defun leo-insert-text-to-file-blind ()
  "Put point either before first level-1 heading or at end of buffer.
Normally one wants to yank to the end of the buffer.
But if it's a polished document rather than an inbox,
then one wants the new text at the top, where its more noticeable.
Function assumes a polished document will have a level-1 near the top."

  (goto-char (point-min))
  (condition-case nil
      (progn
        (re-search-forward "^* ") ; Search for a level-1 headline.
        (goto-char (point-at-bol))
        (insert leo-object-text)
        )
    (error (leo-insert-to-end-of-buffer))
    )
  (leo-text-inserted-to-buffer-path-message)
  )
;; ***** destination = text
;; ****** main defun

(defun leo-refile-text-to-outline ()
  "Refile text to an outline heading in the next window.

Assume that the first line of the target window is the parent
heading. Show the parent's direct child headings. Use `isearch'.
If multiple matches result, pick one with `avy'.

Refile text at point in home window to the bottom of the target heading.
Refiled text may be a line or an outline heading."

  (interactive)

  (select-window (next-window))

  ;; reset target list visibility
  (goto-char (point-min))
  (outline-hide-subtree)
  (outline-show-children 1)
  (outline-hide-body)

  (let ((search-invisible nil))
    (isearch-forward))

  ;; Run Avy if multiple isearch matches.
  ;; Avy doesn't signal a quit, so it is inferred from point.
  (let ((avy-all-windows nil)
        (avy-case-fold-search nil))
    (unless (eq 1 (length (avy--regex-candidates (regexp-quote isearch-string))))
      (goto-char (point-min))
      (avy-isearch)
      (if (eq (point) (point-min))
          (user-error "Quit Avy"))))

  (save-restriction
    (org-narrow-to-subtree)
    (leo-region-ends-n-newlines 2)
    (save-selected-window (select-window (previous-window))
                          (leo-snort-text)
                          )
    (insert leo-object-text)
    (goto-char (point-min))
    )
  (outline-hide-subtree)
  )

;; *** refile up
;; **** main defun

;;;###autoload
(defun leo-refile-up (&optional count)
  "Refile file or text one directory upwards, COUNT times."
  (interactive "p")

  (dotimes (var count)

    (if (eq major-mode 'dired-mode)
        (leo-refile-up-file)
      (leo-refile-up-text))
    (message "Threw up %s times" (1+ var))
    )
  )
;; **** jump height

(defun leo-jump-destination ()
  "Return directory 1-2 above current, depending on ../0-Inbox."

  (concat default-directory

          ;; "Returns ../ unless parent dir is 0-inbox, then ../../"
          (if (leo-parent-dir-inbox-p)
              "../../"
            "../")
          )
  )
;; **** object = text

(defun leo-refile-up-text ()
  "Refile text upwards in the directory tree to the next /0-Inbox."

  (let ((leo-buffer-home (current-buffer))
        (default-directory (leo-jump-destination))
        )
    (leo-snort-text)
    (leo-create-open-inbox-file)
    (leo-insert-text-to-file-blind)
    (switch-to-buffer leo-buffer-home) ; because save-current-buffer failed here
    )
  )
;; **** target = file

(defun leo-refile-up-file ()
  "Refile file upwards in the directory tree to the next /0-Inbox."

  (let* ((leo-jump-destination (leo-jump-destination))
         (leo-inbox-dir (concat leo-jump-destination "0-Inbox/"))
         )
    (if (file-exists-p leo-inbox-dir)
        ()
      (mkdir leo-inbox-dir)
      )
    (rename-file (dired-get-filename "no-dir") leo-inbox-dir)
    (message "File refiled to %s" leo-jump-destination)
    )
  (revert-buffer) ; refreshes screen significantly faster than otherwise.
  )
;; *** library
;; **** snort type
;; ***** text mode?

(defun leo-snort-text ()
  "If heading or line of text to leo-snort-line variable."
  (cond ((eq major-mode 'org-mode) (leo-snort-text-org))
        ((-contains-p minor-mode-list 'outshine-mode) (leo-snort-text-outshine))
        ((-contains-p minor-mode-list 'outline-minor-mode) (leo-snort-text-outline))
        (t (leo-snort-line))
        )
  )
;; ***** at heading?

(defun leo-snort-text-org ()
  "Store text. Range stored depends on local context.

If in a an `org' heading, store the heading. Otherwise, store the
line."

  (if (org-at-heading-p) (leo-snort-org-heading)
    (leo-snort-line))
  )
(defun leo-snort-text-outshine ()
  "Store text. Range stored depends on context.

If inside an `outshine' outline heading, store text. Otherwise,
store line."

  (if (outline-on-heading-p) (leo-snort-outshine-heading)
    (leo-snort-line))
  )
(defun leo-snort-text-outline ()
  "Store text. Range stored depends on context.

If in an outline heading, store the heading. Otherwise store
line."

  (if (outline-on-heading-p) (leo-snort-outline-heading)
    (leo-snort-line))
  )
;; ***** heading type?

(defun leo-snort-org-heading ()
  "Store an `org' heading."

  (save-restriction
    (org-narrow-to-subtree)
    (leo-snort-visible)
    )
  )
(defun leo-snort-outshine-heading ()
  "Store an `outshine' heading."

  (save-restriction
    (outshine-narrow-to-subtree)
    (leo-snort-visible)
    )
  )
(defun leo-snort-outline-heading ()
  "Store an outline heading."

  (save-restriction
    (org-narrow-to-subtree)
    (leo-snort-visible)
    )
  )
;; ***** line

(defun leo-snort-line ()
  "Move a line of text to variable `leo-object-text'."

  (if (eq (point-min) (point-max))
      (user-error "%s" "Selected line is empty")
    (setq leo-object-text
          (concat (delete-and-extract-region (line-beginning-position) (line-end-position))
                  "\n"
                  )
          )
    (leo-delete-leftover-empty-line)
    )
  )
;; **** files
;; ***** Find the searched dired entry

(defun leo-search-dired-open ()
  "Open the `dired' file that the user picked.

Use `isearch'. If `isearch' returns multiple matches, then
use `avy' to pick one."

  (if (string-equal major-mode "dired-mode")
      nil
    (user-error "%s" "Mode must be Dired"))

  (goto-char (point-min))

  (let ((inhibit-message t))
    (dired-hide-details-mode))

  (let ((search-invisible nil))
    (isearch-forward))

  ;; Run Avy if multiple isearch matches.
  ;; Avy doesn't signal a quit, so it is inferred from point.
  (let ((avy-all-windows nil)
        (avy-case-fold-search nil))
    (unless (eq 1 (length (avy--regex-candidates (regexp-quote isearch-string))))
      (goto-char (point-min))
      (avy-isearch)
      (if (eq (point) (point-min))
          (user-error "Quit Avy"))))

  (dired-find-file)
  )
;; ***** check whether immediate parent dir is "0-Inbox"

(defun leo-parent-dir-inbox-p ()
  "Return t if parent dir is 0-Inbox."

  (equal
   (file-name-nondirectory (directory-file-name default-directory)) ; Return parent directory.
   "0-Inbox")
  )
;; ***** Inbox.org creation
;; ****** Create open Inbox.org

(defun leo-create-open-inbox-file ()
  "If no Inbox.org, make it and insert *** offset."

  (let* ((leo-inbox-file-path (concat default-directory "Inbox.org"))
         (leo-inbox-file-buffer (find-buffer-visiting leo-inbox-file-path)))

    (cond (leo-inbox-file-buffer (set-buffer leo-inbox-file-buffer)) ; select buffer if exists
          ((file-exists-p leo-inbox-file-path) (find-file leo-inbox-file-path)) ; open file if exists
          ;; else create and open file
          (t (progn (f-touch "Inbox.org")
                    (find-file leo-inbox-file-path)
                    (insert leo-inbox-file-header)
                    (goto-char (point-min))
                    (org-cycle-hide-drawers 1)
                    (goto-char (point-max))
                    )
             )
          )
    )
  )
;; ******* customization

(defcustom leo-inbox-file-header "*** Inbox.org\n:PROPERTIES:\n:VISIBILITY: children\n:END:\n\n"
  "Header inserted into new Inbox.org files created by `leo-refile-text' and `leo-refile-up-text'."
  :type '(string)
  :group 'trs)
;; ** utilities
;; *** leo-delete-this-buffer-and-file

;;;###autoload
(defun leo-delete-this-buffer-and-file ()
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
;; *** org links
;; **** Store link and fold the PROPERTIES drawer

;;;###autoload
(defun leo-store-link-fold-drawer ()
  "Store an org link to a heading, and fold the drawer."
  (interactive)

  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-store-link nil t) ; Without interactive=t arg, no org link gets created.
      (org-previous-visible-heading 1)
      (org-cycle-hide-drawers 1)
      )
    )
  )
;; **** create Zinks.org

;;;###autoload
(defun leo-dired-zinks ()
  "Make Zinks.org.  Insert org-id link.

Link title's path is relative to `vc-root-dir' if present,
else `user-home-directory'."
  (interactive)

  (let ((zinks-filename (concat default-directory "Zinks.org"))
        )
    (if (file-exists-p zinks-filename)
        (user-error "%s" "Zinks.org already exists")
      (find-file zinks-filename)
      (insert (concat "*** "
                      (file-relative-name (file-name-directory buffer-file-name)
                                        (cond ((vc-root-dir) (vc-root-dir))
                                              (user-home-directory user-home-directory) ; Spacemacs variable. If missing, no problem.
                                              )
                                        )
                      "\n\n\n"
                      )
              )
      (leo-store-link-fold-drawer)
      (goto-char (point-max))
      )
    )
  )
;; *** duplicate heading to other window

;;;###autoload
(defun leo-duplicate-heading-to-other-window ()
  "Append heading at point to end of next window's buffer."
  (interactive)

  (save-restriction
    (org-narrow-to-subtree)
    (leo-region-ends-n-newlines 1)
    (let ((home-buffer (current-buffer))
          )
      (save-selected-window
        (select-window (next-window))
        (leo-region-ends-n-newlines 2)
        (insert-buffer-substring home-buffer)
        )
      )
    )
  )
;; ** library

;; *** heading ends n newlines

(defun leo-region-ends-n-newlines (n)
  "Make region end in N newlines. Set point to end of region."

  (if (>= n 0)
      ()
    (user-error "N is too small: %s" n)
    )

  (let ((m (- n 1)))
    (goto-char (point-max))
    (if (bolp)
        (if (= n 0)
            (progn (org-N-empty-lines-before-current n)
                   (delete-char -1))
          (org-N-empty-lines-before-current m)
          )
      (insert (make-string n ?\n))
      )
    )
  (goto-char (point-max))
  )
;; *** snort visible region

(defun leo-snort-visible ()
  "Move region to `leo-object-text'.  Widen.  Delete empty line."

  (leo-region-ends-n-newlines 1)
  (setq leo-object-text (delete-and-extract-region (point-min) (point-max)))
  (widen)
  (leo-delete-leftover-empty-line)
  )
;; *** safely delete empty line

(defun leo-delete-leftover-empty-line ()
  "Deletes empty line at point, if there is one."

  (unless (and (bobp) (eobp))
    (if (bobp)
        (delete-char 1)
      (when
          (org--line-empty-p 1) ; (not my) bug: This wrongly returns nil when point is on an empty line at top of buffer.  Hence the workaround.
        (delete-char -1)
        (unless (eobp) (forward-char 1))
        )
      )
    )
  )
;; *** insert at bottom of buffer

(defun leo-insert-to-end-of-buffer ()
  "Add `leo-object-text' text to bottom of target buffer."

  (widen)
  (leo-region-ends-n-newlines 2)
  (insert leo-object-text)
  )
;; *** text inserted confirmation message

(defun leo-text-inserted-to-buffer-path-message ()
  "Report filename that text was inserted to.

Reported path is relative to vd-root-dir or ~/."

  (message "Inserted text into `%s'" (if (vc-root-dir)
                                         (expand-file-name buffer-file-name (vc-root-dir))
                                       (expand-file-name buffer-file-name user-home-directory)
                                       )
           )
  )
;; ** provide

(provide 'trs)
;;; trs.el ends here
