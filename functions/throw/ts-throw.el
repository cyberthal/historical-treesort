;; * ts-throw.el
(provide 'ts-throw)
;; * code

;; ** main defun

(defun ts-throw ()
  "Throw text or dired item to a target."

  (interactive)
  (ts-throw-q-is-source-buffer-dired)
  )

;; ** flow control dispatcher

;; *** Is source buffer dired?

(defun ts-throw-q-is-source-buffer-dired ()
  "Determines whether throwing text or files."

(if (eq major-mode 'dired-mode)
    (ts-throw-file)
  (ts-throw-text)
  )
)

;; *** ts-throw-file

(defun ts-throw-file ()
  "Exists for code legibility."
(ts-throw-q-is-file-target-buffer-dired)
)

;; **** Throwing file.  Is target buffer dired? 

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

;; *** ts-throw-text

(defun ts-throw-text ()
  "Code legibility transformation."
  (ts-throw-q-is-text-target-buffer-dired)
)

;; **** Throwing text. Is target buffer type dired? 

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


;; ** require

(require 'ts-library)
(require 'ts-throw-file-to-dired)
(require 'ts-throw-library)
(require 'ts-throw-text-to-dired)
(require 'ts-throw-text-to-outline)
