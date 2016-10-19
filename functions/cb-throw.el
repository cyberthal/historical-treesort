;; * Code body

;; ** sub-functions
;; *** cb-Current-Heading-Level

;; Defines a function that cb-throw depends upon.  Returns the number of stars in the top-level heading of the narrowed region.
(defun cb-Current-Heading-Level ()
  "Returns current outline heading level.
Bounces point to parent heading & counts stars."
  (save-excursion
    (other-window 1)
    (goto-char (point-min)) ; goes to top of narrowed region
    (skip-chars-forward "*") ; move point to the end of the stars, returns number of chars moved forward.
    )			     ; end save excursion
  )			     ; end defun

;; *** cb-Prints-Starry-String

(defun cb-Print-Starry-String ()
  "Print starry string prefix of target heading 
for the search string."
(concat
       "\n" ;; newline to avoid grabbing subheading matches
       (make-string
	(+
	 (cb-Current-Heading-Level) 0)
	?*)
       " ")	    
)

;; *** cb-Goto-Target

(defun cb-Goto-Target-Heading ()
  "Goes to targeted heading"

  ;; jump to top left of other windows
    (other-window 1)
    (goto-char (point-min)) 

    ;; find target heading
    (search-forward 
     (concat 
      (cb-Print-Starry-String)
      (message "%s" x) ; appends user-entered prefix to search string
      ))	       ; end search-forward 
    ) ; end defun

;; ** cb-throw

(defun cb-throw (x)
  "Appends current line to the body of a selected child heading.  Assumes parent heading is at the top of the visible region.

Prompts user for input.  Asks for enough letters from the beginning of the target child heading to distinguish it from the other immediate children of the parent heading.  Searches for a simple string.  Takes the first match.  Does not take any other arguments.

If no match found, fails with an error, and does not kill the line."

  (interactive "sEnter target heading's unique prefix: ")

  ;; fail with an error if user passes a bad heading prefix BEFORE deleting the line to be thrown.  Avoids data loss
  (save-excursion   
    (cb-Goto-Target-Heading))

  ;; safe to proceed.  commence throwing the line.
  (save-excursion   

    ;; cut the object line
    (beginning-of-line) 
    (kill-line 1)	
;    (delete-char -1)	; delete trailing newline

    (cb-Goto-Target-Heading)
    
    ; go to end of target heading, add a blank line, and yank.
    (org-narrow-to-subtree)
    (goto-char (point-max))    
    (newline)
    (yank)
  
    ) ; end of main save excursion
  )   ; end of Throw defun
 

(provide 'cb-throw)
