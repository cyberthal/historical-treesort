;; * cb-throw.el
(provide 'cb-throw)
;; * code

;; ** main defun

(defun cb-throw ()
  "Throw text or dired item to a target."

  (interactive)
  (cb-throw-q-is-source-buffer-dired)
  )

;; ** flow control dispatcher

;; *** Is source buffer dired?

(defun cb-throw-q-is-source-buffer-dired ()
  "Determines whether throwing text or files."

(if (eq major-mode 'dired-mode)
    (cb-throw-file)
  (cb-throw-text)
  )
)

;; *** cb-throw-file

(defun cb-throw-file ()
  "Exists for code legibility."
(cb-throw-q-is-file-target-buffer-dired)
)

;; **** Throwing file.  Is target buffer dired? 

(defun cb-throw-q-is-file-target-buffer-dired ()
  "If target buffer not dired, return error."

(other-window 1)
(if (eq major-mode 'dired-mode)

    ;; then
    (progn
      (other-window 1)
      (cb-throw-file-to-dired)
      )

  ;; else
  (print "Target buffer type must be dired")
  )

)

;; *** cb-throw-text

(defun cb-throw-text ()
  "Code legibility transformation."
  (cb-throw-q-is-text-target-buffer-dired)
)

;; **** Throwing text. Is target buffer type dired? 

(defun cb-throw-q-is-text-target-buffer-dired ()
  "Throwing text. Is target buffer type dired?"

(other-window 1)
(if (eq major-mode 'dired-mode)

;; then
(progn
    (other-window 1)
    (cb-throw-text-to-dired)
    )

;; else
(progn
(other-window 1)
  (call-interactively
   'cb-throw-text-to-outline)
  )

  ) ; end if
) ; end defun


;; ** require

(require 'cb-library)
(require 'cb-throw-file-to-dired)
(require 'cb-throw-library)
(require 'cb-throw-text-to-dired)
(require 'cb-throw-text-to-outline)
