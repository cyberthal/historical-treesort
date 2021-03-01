;;; treefactor ERT test file     -*- lexical-binding: t; -*-

(ert-deftest treefactor-org-refactor-heading ()
  (UNWIND ALL 
          (COPY START.ORG TO TARGET.ORG)
          (OPEN TARGET.ORG)
          (SOLE WINDOW)
          (treefactor-org-refactor-heading)
          (should (EQUAL START.ORG END.ORG))
          (REVERT START.ORG BUFFER SOMEHOW)
          )
  )


(ert-deftest treefactor-refresh-org-search-scope ()

  (let ( (org-agenda-files) (org-id-extra-files)
         (treefactor-org-agenda-dir (concat default-directory "Scratch/Org/Agenda"))
         (treefactor-org-id-extra-dir (concat default-directory "Scratch/Org/Extra")))
    (should
     (equal '(("/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Agenda/1/2/3.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Agenda/1/2.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Agenda/1.org")
              ("/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Extra/1/2/3.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Extra/1/2.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Extra/1.org"))
            (progn (treefactor-refresh-org-search-scope) (list org-agenda-files org-id-extra-files))))))
