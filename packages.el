;;; packages.el --- cyborganize layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Leo Joseph Buchignani III <leo3@vbox-manjaro>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst cyborganize-packages
  '()
  "The list of Lisp packages required by the cyborganize layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(with-eval-after-load 'org

  (push "~/.emacs.d/private/cyborganize/functions/" load-path)
  (push "~/.emacs.d/private/cyborganize/functions/throw/" load-path)
  (require 'cb-throw)
  (require 'cb-macros)
  (require 'cb-library)
  )

;; ensure don't isearch invisible text in dired mode
(add-hook 'dired-mode-hook
          (lambda ()
            (make-local-variable 'search-invisible)
            (setq search-invisible nil)))

;;; packages.el ends here
