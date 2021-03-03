;;; treefactor ERT test file     -*- lexical-binding: t; -*-

(ert-deftest treefactor-org-refactor-heading ()
  (save-window-excursion
  (switch-to-buffer "treefactor-org-refactor-heading-test.org" t)
  (erase-buffer)
  (insert-file-contents (concat default-directory "Scratch/Treefactor/Org-refactor-heading/Start.org"))
  (goto-char (search-forward "**** target heading"))
  (delete-other-windows)
  (org-mode)
  (treefactor-org-refactor-heading)
  (widen)
  (should (equal (buffer-string)
                 (with-temp-buffer (insert-file-contents
                                    (concat default-directory "Mock/Treefactor/Org-refactor-heading/End.org"))
                                   (buffer-string))))
  (kill-buffer "treefactor-org-refactor-heading-test.org")))


(ert-deftest treefactor-refresh-org-search-scope ()

  (let ( (org-agenda-files) (org-id-extra-files)
         (treefactor-org-agenda-dir (concat default-directory "Mock/Org/Agenda"))
         (treefactor-org-id-extra-dir (concat default-directory "Mock/Org/Extra")))
    (should
     (equal '(("/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Agenda/1/2/3.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Agenda/1/2.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Agenda/1.org")
              ("/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Extra/1/2/3.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Extra/1/2.org" "/Users/leo3/1-Mansort/2-Other-text/Git-/Hub/Emacs/My/Treefactor/Dev/2-tEst/Scratch/Org/Extra/1.org"))
            (progn (treefactor-refresh-org-search-scope) (list org-agenda-files org-id-extra-files))))))
