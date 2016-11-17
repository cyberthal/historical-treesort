;; * cb-throw-org-to-org.el
;; * Code

;; ** sub-functions
;; *** cb-Target-Max-Heading-Level

(defun cb-Target-Max-Heading-Level ()
  "Returns target window's max outline heading level.
Bounces point to target top visible heading & counts stars."

  ;; net side effect: switches to other window, top heading

    (other-window 1)
    (goto-char (point-min)) ; goes to top of narrowed region
    (skip-chars-forward "*") ; move point to the end of the stars, returns number of chars moved forward.
    
  )			     ; end defun

;; *** cb-Print-Starry-String

(defun cb-Print-Starry-String ()
  "Print starry string prefix of target heading 
for the search string."

  ;; net side effect: switches to other window, top heading

(concat
       "\n" ;; newline to avoid grabbing subheading matches
       (make-string
	(+
	 (cb-Target-Max-Heading-Level) 1)
	?*)
       " ")
)

;; *** cb-Goto-Target-Heading

(defun cb-Goto-Target-Heading ()
  "Goes to targeted heading"

;; net effect - switches to other window, then jumps to targeted heading

  ;; jump to top left of other windows
;    (other-window 1)
;    (goto-char (point-min)) 

    ;; find target heading
    (search-forward 
     (concat 
      (cb-Print-Starry-String)
      (message "%s" x) ; appends user-entered prefix to search string
      ))	       ; end search-forward 
    ) ; end defun

;; ** cb-throw-text-to-outline

(defun cb-throw-text-to-outline (x)
  "Appends current line to the body of a selected child heading.  Assumes parent heading is at the top of the visible region.

Prompts user for input.  Asks for enough letters from the beginning of the target child heading to distinguish it from the other immediate children of the parent heading.  Searches for a simple string.  Takes the first match.  Does not take any other arguments.

If no match found, fails with an error, and does not kill the line."

  (interactive "sEnter target heading's unique prefix: ")

  ;; fail with an error if user passes a bad heading prefix BEFORE deleting the line to be thrown.  Avoids data loss

(save-excursion
  (cb-Goto-Target-Heading)
  )
(other-window 1)

  ;; safe to proceed.  commence throwing the line.

    ;; cut the object line
    (beginning-of-line)
    (kill-line 1)

    (cb-Goto-Target-Heading)

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
 

(provide 'cb-throw-org-to-org)
