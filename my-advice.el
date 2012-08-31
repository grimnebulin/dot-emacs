(defadvice open-line (around vi-style-open-line)
  "Make open-line behave more like vi."
  (beginning-of-line)
  ad-do-it
  (indent-according-to-mode))

(defvar *recenter-fraction* 0.5
  "*The recenter-proportionally advice will recenter the screen by
putting the current line this far down the window.")

(defadvice recenter (before recenter-proportionally)
  (or (ad-get-arg 0)
      (ad-set-arg 0 (truncate (* *recenter-fraction* (window-body-height))))))

(defadvice ucs-insert (before use-ido-completing-read)
  (interactive (list (ido-read-char-by-name "Unicode (name or hex): "))))

(defadvice gnus (after cd-to-home-dir)
  (cd "~"))

(defadvice bookmark-jump (around spawn-shell-if-prefix)
  "If a prefix argument is supplied, spawn a shell in the bookmark's directory instead."
  (if current-prefix-arg
      (shell-in-bookmark-directory (ad-get-arg 0))
    ad-do-it))

(defadvice insert-register (before invert-prefix-arg)
  "Invert the sense of the prefix argument to insert-register."
  (ad-set-arg 1 (not (ad-get-arg 1))))

(defmacro with-gensyms (names &rest body)
  `(let ,(loop for name in names collect (list name (gensym)))
     ,@body))

(defadvice shell-command (before allow-multiple-asynchronous-commands)
  (when (and (string-match "&[ \t]*\\'" (ad-get-arg 0))
             (not (ad-get-arg 1)))
    (ad-set-arg 1 (generate-new-buffer-name "*Async Shell Command*"))))

(defadvice dired-do-flagged-delete (around delete-recursively-if-prefix)
  "Sets dired-recursive-deletes to 'always for the duration of this command
if a prefix argument is present."
  (let ((dired-recursive-deletes
         (if current-prefix-arg 'always dired-recursive-deletes)))
    ad-do-it))
