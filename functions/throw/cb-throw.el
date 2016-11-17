;; * cb-throw.el

;; * code

;; ** flow control dispatcher

;; *** Is source buffer dired?

(defun cb-throw-q-is-source-buffer-dired ()
  "Determines whether throwing text or files."

(if (eq major-mode 'dired-mode)
    (then cb-throw-file)
  (else cb-throw-text)
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
    (then cb-throw-file-to-dired)
  (else print error "Target buffer type must be dired")
  )
(other-window 1)
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
    (then cb-throw-text-to-dired)
  (else cb-throw-text-to-outline)
  )
(other-window 1)
)

;; ** human notes
;; *** dispatch permutations

org-org
org-dired
dired-dired
dired-org = error

;; *** undefined functions

cb-throw-file-to-dired
