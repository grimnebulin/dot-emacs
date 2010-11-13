(defun start-piped-process (&rest args)
  (let ((process-connection-type nil))
    (apply 'start-process args)))

(defun join-lines ()
  (interactive)
  (end-of-line)
  (delete-region (point) (line-beginning-position 3)))

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

(defmacro set-associations (alist &rest new-assocs)
  (let ((new-assoc (gensym))
        (found (gensym)))
    `(loop for ,new-assoc in ',new-assocs do
       (let ((,found (assoc (car ,new-assoc) ,alist)))
         (if ,found
             (setcdr ,found (cdr ,new-assoc))
           (setq ,alist (cons ,new-assoc ,alist)))))))

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

(defun stackoverflow-copy-code-snippet (begin end)
  (interactive "r")
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) 4)
      (clipboard-kill-ring-save (point-min) (point-max)))))

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

(defun kill-this-buffer-and-maybe-associated-file ()
  (interactive)
  (let ((buffer (current-buffer))
        (file-name (or (buffer-file-name)
                       (error "Buffer is not visiting a file"))))
    (kill-buffer buffer)
    (when (and (not (buffer-live-p buffer))
               (y-or-n-p (concat "Delete file " file-name "? ")))
      (delete-file file-name))))

(defun upcase-region-or-characters (arg)
  (interactive "p")
  (upcase-region (point) (if mark-active (mark) (+ arg (point)))))

(defun downcase-region-or-characters (arg)
  (interactive "p")
  (downcase-region (point) (if mark-active (mark) (+ arg (point)))))

(defun crontab ()
  (interactive)
  (shell-command "EDITOR=emacsclient crontab -e &"))
