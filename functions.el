;; (defun start-piped-process (&rest args)
;;   (let ((process-connection-type nil))
;;     (apply 'start-process args)))

;; (defun join-lines ()
;;   (interactive)
;;   (end-of-line)
;;   (delete-region (point) (line-beginning-position 3)))

(defun nonempty-p (sequence)
  (and (< 0 (length sequence)) sequence))

(defun princ-to-string (x)
  (format "%s" x))

(defun pipe (value &rest funcs)
  (while (and value funcs)
    (setq value (funcall (car funcs) value)
          funcs (cdr funcs)))
  value)

(defmacro string-or (&rest exprs)
  (let ((x (gensym)))
    `(loop for ,x in ',exprs
           thereis (pipe ,x 'eval 'princ-to-string 'nonempty-p)
           finally return "")))

(defmacro global-set-key* (keys &rest body)
  `(global-set-key ,keys (lambda () (interactive) ,@body)))

(defmacro add-hook* (hook &rest body)
  `(add-hook ,hook (lambda () ,@body)))

;; 

(defun other-window-delete-rest (arg)
  "Goes to the next window with other-window, then makes that window
the only visible window.  With a prefix argument, kills the current
buffer first."
  (interactive "P")
  (if arg (kill-buffer nil))
  (other-window 1)
  (delete-other-windows))

;;

(defun perlmod (module)
  "Reads the name of a Perl module, then loads the file that implements
that module, if it exists."
  (interactive "sPerl module: ")
  (let ((path (perl-module-path module)))
    (if (not (and (string< "" path) (file-exists-p path)))
        (error "Could not locate module file")
      (find-file path)
      (message "%s" path))))

(defun perl-module-path (module)
  "Returns the path to the Perl file that implements the given module."
  (let ((path (concat (replace-regexp-in-string "::" "/" module) ".pm")))
    (shell-command-to-string
     (concat "perl -M" module " -e 'print $INC{ shift() }' " path))))

;;

(defmacro ascend-directories-until (&rest body)
  "Steps upwards from default-directory to the root directory.

At each step, the local variables dirname and filename are set to the
current path and current directory, respectively, and \"body\" is
evaluated as the body of a cond statement; if the statement returns
true, the loop halts."
  (let ((done (make-symbol "done"))
        (dir  (make-symbol "dir"))
        (dfn  (make-symbol "dfn")))
    `(let (,done (,dir (expand-file-name default-directory)))
       (while (not (or ,done (equal ,dir "/")))
         (let* ((,dfn (directory-file-name ,dir))
                (dirname (file-name-directory ,dfn))
                (filename (file-name-nondirectory ,dfn)))
           (setq ,done (cond ,@body)
                 ,dir (file-name-as-directory dirname))))
       (or ,done (message "ascend-directories-until: nothing happened")))))

;;

(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(defun maximize-frame ()
  (interactive)
  ;; Only works on this Mac!
  (let ((frame (selected-frame)))
    (set-frame-position frame 0 0)
    (set-frame-size frame 188 52)))

(defmacro to-system-clipboard (&optional name &rest body)
  "Execute body in a temporary buffer, then copy the accessible portion
of the buffer to the system clipboard."
  `(with-temp-buffer
     (save-excursion ,@(if (stringp name) body (cons name body)))
     (clipboard-kill-ring-save (point-min) (point-max))
     ,@(when (stringp name) (list (message "%s copied to system clipboard" name)))))

(defun stackoverflow-copy-code-snippet (begin end)
  (interactive "r")
  (let ((buffer (current-buffer)))
    (to-system-clipboard
     "Code snippet"
     (insert-buffer-substring-no-properties buffer begin end)
     (indent-rigidly (point-min) (point-max) 4))))

(defvar shell-command-with-?-expansion-history nil)

(defun buffer-file-name-or-error ()
  (or (buffer-file-name) (error "Not visiting a file")))

(defun shell-command-with-?-expansion (command &optional output-buffer)
  (interactive
   (progn
     (buffer-file-name-or-error)
     (list (read-from-minibuffer
            "Shell command (with ? expansion): "
            nil nil nil 'shell-command-with-?-expansion-history)
           current-prefix-arg)))
  (shell-command
   (let ((quoted-file-name (shell-quote-argument (buffer-file-name-or-error))))
     (while (string-match "\\(^\\|[ \t]\\)\\(\\?\\)\\([ \t]\\|$\\)" command)
       (setq command (replace-match quoted-file-name t t command 2)))
     command)
   output-buffer))

(defun kill-this-buffer-and-associated-file ()
  (interactive)
  (let ((buffer (current-buffer))
        (file-name (buffer-file-name-or-error)))
    (kill-buffer buffer)
    (and (not (buffer-live-p buffer))
         (delete-file file-name))))

(defun upcase-region-or-characters (arg)
  (interactive "p")
  (upcase-region (point) (if mark-active (mark) (+ arg (point)))))

(defun downcase-region-or-characters (arg)
  (interactive "p")
  (downcase-region (point) (if mark-active (mark) (+ arg (point)))))

(defun killdir ()
  (interactive)
  (if (not default-directory)
      (error "default-directory is nil")
    (kill-new default-directory)))

(defun send-last-shell-to-this-directory (prefix)
  "Switch to the most recently visited shell buffer, and issue a \"cd\"
command to move it to the default directory of the buffer which was
current when this command was invoked."
  (interactive "p")
  (let* ((buffer (or (loop for buffer being the buffers
                           if (with-current-buffer buffer
                                (eq major-mode 'shell-mode))
                           return buffer)
                     (error "No shell buffer available")))
         (proc (get-buffer-process buffer))
         (dir default-directory)
         (command (concat "cd " dir "\n")))
    (or prefix (kill-buffer nil))
    (switch-to-buffer buffer)
    (unless comint-process-echoes
      (insert command))
    (sit-for 0) ; perform redisplay
    (comint-send-string proc command)
    (set-marker (process-mark proc) (point))
    (cd dir)))

(defun crontab ()
  (interactive)
  (shell-command "EDITOR=emacsclient crontab -e &"))

(defun toggle-case ()
  (interactive)
  (let ((case-fold-search nil))
    (when (search-forward-regexp "\\=\\([A-Z]\\)\\|\\=\\([a-z]\\)" nil t)
      (replace-match
       (if (match-string 1)
           (downcase (match-string 1))
         (upcase (match-string 2)))
       t)
      (backward-char 1))))
