(make-variable-buffer-local 'do-not-indent-after-open-line)

(defadvice open-line (around vi-style-open-line)
  "Make open-line behave more like vi."
  (if (not (called-interactively-p 'any))
      ad-do-it
    (beginning-of-line)
    ad-do-it
    (unless do-not-indent-after-open-line
      (indent-according-to-mode))))

(defvar *recenter-fraction* 0.5
  "*The recenter-proportionally advice will recenter the screen by
putting the current line this far down the window.")

(defadvice recenter (before recenter)
  (or (ad-get-arg 0)
      (ad-set-arg 0 (truncate (* *recenter-fraction* (window-body-height))))))

(defadvice insert-char (before use-ido-completing-read)
  (interactive (list (ido-read-char-by-name "Unicode (name or hex): "))))

(defadvice gnus (after cd-to-home-dir)
  (when (called-interactively-p 'any)
    (cd "~")))

(defadvice bookmark-jump (around spawn-shell-if-prefix)
  "If a prefix argument is supplied, spawn a shell in the bookmark's directory instead."
  (when (called-interactively-p 'any)
    (if current-prefix-arg
        (shell-in-bookmark-directory (ad-get-arg 0))
      ad-do-it)))

(defadvice insert-register (before invert-prefix-arg)
  "Invert the sense of the prefix argument to insert-register."
  (when (called-interactively-p 'any)
    (ad-set-arg 1 (not (ad-get-arg 1)))))

(defadvice dired-do-flagged-delete (around delete-recursively-if-prefix)
  "Sets dired-recursive-deletes to 'always for the duration of this command
if a prefix argument is present."
  (when (called-interactively-p 'any)
    (let ((dired-recursive-deletes
           (if current-prefix-arg 'always dired-recursive-deletes)))
      ad-do-it)))

(defadvice magit-key-mode-popup-logging (after show-all-by-default)
  "Turn on the --all logging option by default."
  (add-to-list 'magit-key-mode-current-options "--all")
  (magit-key-mode-redraw 'logging))

nil
