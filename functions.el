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

(defun buffer-file-name* ()
  (or (buffer-file-name) (error "Not visiting a file")))

(defun shell-command-with-?-expansion (command &optional output-buffer)
  (interactive
   (progn
     (buffer-file-name*)
     (list (read-from-minibuffer
            "Shell command (with ? expansion): "
            nil nil nil 'shell-command-with-?-expansion-history)
           current-prefix-arg)))
  (shell-command
   (let ((quoted-file-name (shell-quote-argument (buffer-file-name*))))
     (while (string-match "\\(^\\|[ \t]\\)\\(\\?\\)\\([ \t]\\|$\\)" command)
       (setq command (replace-match quoted-file-name t t command 2)))
     command)
   output-buffer))

(defun kill-this-buffer-and-associated-file ()
  (interactive)
  (let ((buffer (current-buffer))
        (file-name (buffer-file-name*)))
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

(defconst +cycle-single-quotes+
  '("''" "q()" "q{}" "q//" "q[]"))

(defconst +cycle-double-quotes+
  '("\"\"" "qq()" "qq{}" "qq//" "qq[]"))

(defun cycle-perl-quotes (pos)
  (interactive "d")
  (or (eq 'font-lock-string-face (get-text-property pos 'face))
      (error "Not within a string"))
  (let* ((start (or (previous-single-property-change pos 'face)
                    (error "Can't find beginning of string")))
         (end   (or (    next-single-property-change pos 'face)
                    (error "Can't find end of string")))
         (string (buffer-substring-no-properties (1+ start) (1- end)))
         (delims (format "%s%s%c%c"
                         (if (equal (char-before (1- start)) ?q) "q" "")
                         (if (equal (char-before     start ) ?q) "q" "")
                         (char-after start)
                         (char-before end)))
         list
         (tail (or (member delims (setq list +cycle-single-quotes+))
                   (member delims (setq list +cycle-double-quotes+))
                   (error "Can't determine string delimiters")))
         (these-quotes (first tail))
         (next-quotes (or (loop for q in (append (rest tail) list)
                                while (not (eq q these-quotes))
                                if (not (string-match
                                         (format "[%s%s]"
                                                 (substring q -1)
                                                 (substring q -2 -1))
                                         string))
                                return q)
                          (error "No appropriate delimiters"))))
    (save-excursion
      (goto-char end)
      (delete-backward-char 1)
      (insert (substring next-quotes -1))
      (goto-char start)
      (delete-char 1)
      (delete-backward-char (- (length these-quotes) 2))
      (insert (substring next-quotes 0 -1)))))

(require 'perl-mode)
(define-key perl-mode-map [(super q)] 'cycle-perl-quotes)

;; Ganked from somewhere.

;; someday might want to rotate windows if more than 2 of them

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

;; Ganked from somewhere.

;;
;; Never understood why Emacs doesn't have this function.
;;

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
	(progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Ganked from somewhere.

;;
;; Never understood why Emacs doesn't have this function, either.
;;

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
 	(delete-file filename)
 	(set-visited-file-name newname)
 	(set-buffer-modified-p nil)
 	t))))
