;; * My notes
;; TODO add iswitchb style fuzzy autocomplete.  add range support. 
;;  will have to use an iterative counter on the search function rather than repeating it twice, to permit flexibility on exactly what is searched for.  so can support functionality in other modes ala Outshine.  but really, I'm doing away with it entirely in favor of fuzzy autocomplete.  so may as well go straight to that. 
;; Does snort repeat easily? yes, it can repeat with repeat-complex-command, then repeat.  cool.  C-x ESC ESC then C-x z then z z z 
;; TODO todo: somehow allow it not add extra spaces if not needed, but do add if needed.  I'm torn on this.  It's probably not hard to clean up the extra blank lines - DEFINITELY not hard.  So maybe only don't add extra lines if you're tossing a region.  This goes back to adding region support, then. 
;; TODO: make intro better.  See for reference:
;; https://github.com/xahlee/xah-find/blob/master/xah-find.el
;; https://github.com/xahlee/xah-find/blob/master/README.org
;; https://learnxinyminutes.com/docs/elisp/

;; * Introduction

;; cb-line-snort.el replicates the "Throw" functionality of the Windows
;; application "BrainStorm".

;; BrainStorm is an outliner that allows rapid text sorting into nested
;; hierarchical layers. It is hosted at
;; http://www.brainstormsw.com/bvo.html

;; Org-Mode already has outliner functionality.  It refiles headings
;; well.  However, I have not found anything in Org-Mode or Emacs that
;; matches BrainStorm's ability to rapidly "throw" individual lines to
;; the appropriate subheadings of an outline.

;; Refiling headings is not an adequate substitute for refiling lines.
;; Transforming every single line into a heading generates a huge mess.
;; This task should be easy, fast and fun, not painful.

;; Thanks to the power of Elisp, cb-line-toss actually improves on the
;; BrainStorm's strongest capabilities.

;; ** Instructions

;; First, you should bind cb-throw to your preferred keychord.  It is
;; the only command you will need to s(n)ort lines.

;; Next, you should understand how cb-throw works.  Here's the
;; human-friendly version:

;; cb-throw will kill the whole line at point.  It will ask you for the
;; first letter(s) of your target child heading.  Then it will append
;; the line to that heading as an invisible background operation.

;; *** The long version

;; Here is a more technical explanation, so that you can avoid
;; surprises:

;; 1. cb-throw believes that the "parent" outline heading is at the top
;; of the visible buffer.  You should narrow the region to your desired
;; parent heading before using cb-throw.

;; 2. cb-throw will only throw to an immediate child of the parent
;; heading.  This prevents you from unknowingly sending a line to a
;; buried heading with a similar prefix.

;; 3. You can still throw an "interception" if you use a prefix that is
;; shared by another immediate child heading that is positioned before
;; your intended target.  So be wary of single-letter prefixes, and be
;; careful when throwing to a crowd of potential targets.  (The Windows
;; BrainStorm application only allowed 6 simultaneous targets.
;; Sometimes the brain works faster when it has fewer options to
;; juggle.  However, cb-throw sets no limit on the number of targets.)

;; 4. If cb-throw cannot find a child heading that matches your prefix,
;; it will echo a "Search failed:" error in the minibuffer.  Your line
;; will not be killed.

;; 5. You can grab lines from any level of the outline.

;; 6. cb-throw will not disturb your view.  Exception - if you throw to
;; a collapsed heading, the thrown line will not be hidden until you
;; refresh the visibility for that heading.  If this becomes bothersome
;; due to large number of lines, use C-c C-k
;; org-kill-note-or-show-branches to re-hide everything.

;; For further information, see the extensively commented source code.
;; This is my first program!

;; ** Roadmap and Tips

;; Org-Mode's default settings offer reasonably good outline visibility
;; controls, but the defaults are not perfect.

;; For example, the widen + outline-up-heading + org-narrow-to-subtree
;; process is painfully cumbersome.  This can be corrected by recording
;; and keybinding a simple keyboard macro.

;; Also, the oddly-named obscure C-c C-k org-kill-note-or-show-branches
;; is a great way to clean up the outline view during sorting.

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

(defun cb-Goto-Target ()
  "Goes to targeted heading"

      ;; move point to other window top of visible section
    (other-window 1)
    (goto-char (point-min)) 

    (search-forward ; moves point to middle of targeted heading 
     (concat ; tested with print.  it concatenates despite odd echo. 
      (cb-Print-Starry-String)
      (message "%s" x) ; appends user-entered prefix to search string
      ))	       ; end search-forward 
    ) ; end defun

;; ** Brainstorm-style Throw command for Org-Mode 

(defun cb-throw (x)
  "Appends current line to the body of a selected child heading.  Assumes parent heading is at the top of the visible region.

Prompts user for input.  Asks for enough letters from the beginning of the target child heading to distinguish it from the other immediate children of the parent heading.  Searches for a simple string.  Takes the first match.  Does not take any other arguments.

If no match found, fails with an error, and does not kill the line."
  (interactive "sEnter target heading's unique prefix: ")

  ;; fail with an error if user passes a bad heading prefix BEFORE deleting the object line.  Avoids data loss!
  (save-excursion   
    (cb-Goto-Target))

  ;; Ok itz safe.  Now we edit for realz. 
  (save-excursion   
    (beginning-of-line) ; preparing to cut the object line
    (kill-line 1)	; cut the object line
;    (delete-char -1)	; delete trailing newline

    (cb-Goto-Target)
  
    (forward-line ; move forward as many lines as it takes to get to the last line of the heading before the next heading
     ;; counting dem lines, from target heading to next heading.  returns value one less than that.  
     (count-lines (point)	  ; START END args
		  (save-excursion ; jaunt forth to find endpoint
		    (org-forward-heading-same-level 1) ; org command moves intelligently to next heading or end of narrowed region.
		    (point) ; causes excursion sexp to return END location
		    )))

    ;; yank.  this should go after point is moved to correct final line position
    (newline)
    (yank)
  ; yanks the object line

  ; more body goes here
    ) ; end of main save excursion
  )   ; end of Throw defun
 

(provide 'cb-throw)
