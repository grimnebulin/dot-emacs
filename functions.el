;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(defun listify (x)
  (if (listp x)
      x
    (list x)))

(defun flatten (x)
  "Flattens the argument, if it is a list (proper or improper), or else
returns it unchanged."
  (if (atom x)
      x
    (append (listify (flatten (car x)))
            (listify (flatten (cdr x)))
            nil)))

(defmacro aif (test then &rest else)
  (declare (indent 2))
  `(let ((it ,test))
     (if it ,then ,@else)))

(defmacro awhen (test &rest then)
  (declare (indent defun))
  `(let ((it ,test))
     (when it ,@then)))

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
  (let ((x (cl-gensym)))
    `(loop for ,x in ',exprs
           thereis (pipe ,x 'eval 'princ-to-string 'nonempty-p)
           finally return "")))

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
    (format-shell-command-to-string
     "perl -M%s -e 'print $INC{ shift() }' %s" module path)))

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

(defun buffer-file-name* ()
  (or (buffer-file-name) (error "Not visiting a file")))

(defun kill-this-buffer-and-associated-file ()
  (interactive)
  (let ((buffer (current-buffer))
        (file-name (buffer-file-name*)))
    (kill-buffer buffer)
    (when (not (buffer-live-p buffer))
      (delete-file file-name))))

(defun upcase-region-or-characters (arg)
  (interactive "p")
  (upcase-region (point) (if mark-active (mark) (+ arg (point)))))

(defun downcase-region-or-characters (arg)
  (interactive "p")
  (downcase-region (point) (if mark-active (mark) (+ arg (point)))))

;; (defun killdir ()
;;   (interactive)
;;   (if (not default-directory)
;;       (error "default-directory is nil")
;;     (kill-new default-directory)))

(defun is-interactive-shell-buffer (buffer)
  (and (with-current-buffer buffer (eq major-mode 'shell-mode))
       (let ((proc (get-buffer-process buffer)))
         (and proc (not (process-sentinel proc))))))

(defun send-last-shell-to-this-directory (prefix)
  "Switch to the most recently visited shell buffer, and issue a \"cd\"
command to move it to the default directory of the buffer which was
current when this command was invoked."
  (interactive "p")
  (let* ((buffer (or (loop for buffer being the buffers
                           if (is-interactive-shell-buffer buffer)
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
      (delete-char -1)
      (insert (substring next-quotes -1))
      (goto-char start)
      (delete-char 1)
      (delete-char (- 2 (length these-quotes)))
      (insert (substring next-quotes 0 -1)))))

(require 'perl-mode)
(define-key perl-mode-map [(super q)] 'cycle-perl-quotes)

(defun* rotate-windows (&optional backwards (windows (window-list)))
  (interactive "P")
  (cl-flet ((get-meta (w) (cons (window-buffer w) (window-start w)))
            (set-meta (w m) (setf (window-buffer w) (car m)
                                  (window-start  w) (cdr m))))
    (when backwards (setq windows (reverse windows)))
    (loop with first-meta = (and windows (get-meta (first windows)))
          for (this next) on windows
          do (set-meta this (if next (get-meta next) first-meta)))))

(defun swap-windows (&optional backwards)
  (interactive "P")
  (rotate-windows nil (list (selected-window)
                            (if backwards (previous-window) (next-window)))))

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

(defun auto-align-regexp ()
  (interactive)
  (let* ((delim (save-excursion
                  (move-beginning-of-line 1)
                  (or (search-forward-regexp "=>" (line-end-position) t)
                      (search-forward-regexp "="  (line-end-position) t)
                      (error "No auto-alignable strings found on current line"))
                  (match-string 0)))
         (start (save-excursion
                  (while (and (/= (line-beginning-position 1)
                                  (line-beginning-position 0))
                              (save-excursion
                                (move-beginning-of-line 0)
                                (search-forward delim (line-end-position) t)))
                    (forward-line -1))
                  (line-beginning-position 1)))
         (end (save-excursion
                (while (and (/= (line-end-position 1)
                                (line-end-position 2))
                            (save-excursion
                              (move-beginning-of-line 2)
                              (search-forward delim (line-end-position) t)))
                  (forward-line 1))
                (line-end-position 1))))
    (align-regexp start end (concat "\\(\\s-*\\)" (regexp-quote delim)) 1 1)))

(defun no-process-query-on-exit ()
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))

(defun scroll-one-line-up ()
  (interactive)
  (scroll-up 1))

(defun scroll-one-line-down ()
  (interactive)
  (scroll-down 1))

(defun shell-quote-format (string &rest args)
  (apply 'format string
         (mapcar (lambda (x) (shell-quote-argument (format "%s" x))) args)))

(defun format-shell-command (string &rest args)
  (shell-command (apply #'shell-quote-format string args)))

(defun format-shell-command-to-string (string &rest args)
  (shell-command-to-string (apply #'shell-quote-format string args)))

(defun delete-frame-and-buffer ()
  (interactive)
  (and (kill-buffer) (delete-frame)))

(defun ido-read-char-sort-predicate (a b)
  "Order character names first by increasing length, then by lexicographic order."
  (let ((alen (length a))
        (blen (length b)))
    (or (< alen blen) (and (= alen blen) (string-lessp a b)))))

(let (completions)
  (defun ido-read-char-completions ()
    (or completions
        (setq completions
              (sort (cl-delete-if (lambda (s) (= ?< (aref s 0)))
                                  (mapcar 'car (ucs-names)))
                    'ido-read-char-sort-predicate)))))

(defun ido-read-char-by-name (prompt)
  "Replacement for read-char-by-name that uses ido to read character names."
  (let* ((completion-ignore-case t)
         (completions (ido-read-char-completions))
         (ido-enable-flex-matching nil)
	 (input (ido-completing-read prompt completions)))
    (cond
     ((string-match-p "^[0-9a-fA-F]+$" input)
      (string-to-number input 16))
     ((string-match-p "^#" input)
      (read input))
     (t
      (cdr (assoc-string input (ucs-names) t))))))

;; Google search routines from Pascal Bourguignon on gnu.emacs.help.
;; (As amended by Stefan Monnier.)

(defun google-search (search-string)
  "Search a string with Google."
  (interactive "sGoogle Search: ")
  (browse-url
   (format "http://www.google.com/search?as_q=%s&num=50&hl=en&ie=ISO8869-1&btnG=Google+Search&as_epq=&as_oq=&as_eq=&lr=&as_ft=i&as_filetype=&as_qdr=all&as_nlo=&as_nhi=&as_occt=any&as_dt=i&as_sitesearch=&safe=images"
           (url-hexify-string search-string))))

(defalias 'g 'google-search)

(defun google-search-region (start end)
  "Search the text in the region with Google."
  (interactive "r")
  (google-search (buffer-substring-no-properties start end)))

(require 'thingatpt)

(defun google-word-at-point ()
  (interactive)
  (google-search (word-at-point)))

(defun shell-in-directory (dir &optional suffix)
  (setq dir (file-name-directory dir))
  (or (loop for b being the buffers
            if (and (is-interactive-shell-buffer b)
                    (with-current-buffer b (string= dir default-directory)))
            return (switch-to-buffer b))
      (let ((default-directory dir))
        (shell
         (generate-new-buffer-name
          (if suffix (format "*shell-%s*" suffix) "*shell*")))
        (delete-other-windows))))

(defun shell-in-bookmark-directory (bookmark)
  (shell-in-directory
   (cdr (assq 'filename (bookmark-get-bookmark-record bookmark)))
   bookmark))

(defun bookmark-jump-other-frame (bookmark)
  (interactive
   (progn (require 'bookmark)  ; For bookmark-completing-read.
          (list (bookmark-completing-read "Jump to bookmark in other frame"))))
  (select-frame (make-frame))
  (if current-prefix-arg
      (shell-in-bookmark-directory bookmark)
    (bookmark-jump bookmark)))

(defun add-to-hooks (func &rest hooks)
  (dolist (hook hooks) (add-hook hook func)))

(defun comment-copy-of-lines (prefix)
  (interactive "p")
  (let (start end)
    (cond
     ((use-region-p)
      (setq start (save-excursion (goto-char (region-beginning))
                                  (line-beginning-position))
            end   (save-excursion (goto-char (region-end))
                                  (line-end-position))))
     ((>= prefix 0)
      (setq start (line-beginning-position)
            end   (line-end-position prefix)))
     (t
      (setq start (line-beginning-position (1+ prefix))
            end   (line-end-position))))
    (incf end)
    (let ((text (buffer-substring start end)))
      (goto-char end)
      (comment-region start end)
      (save-excursion (insert text)))))

(defmacro with-buffer-visiting-file (file &rest body)
  (let ((fileval  (cl-gensym))
        (existing (cl-gensym))
        (buffer   (cl-gensym)))
    `(let* ((,fileval ,file)
            (,existing (find-buffer-visiting ,fileval))
            (,buffer (if ,existing
                         (make-indirect-buffer
                          ,existing
                          (generate-new-buffer-name
                           (buffer-name ,existing)))
                       (find-file-noselect ,fileval t t))))
       (unwind-protect
           (with-current-buffer ,buffer
             (widen)
             (beginning-of-buffer)
             ,@body)
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             (or ,existing (set-buffer-modified-p nil))
             (kill-buffer)))))))

(defconst +perl-package-regexp+ "[[:upper:]][[:alnum:]]*\\(?:::[[:upper:]][[:alnum:]]*\\)+")

(require 'hippie-exp)

(defun try-complete-perl-package-name (old)
  (when (not old)
    (he-init-string (save-excursion (skip-chars-backward ":[:alnum:]") (point)) (point))
    (setq he-expand-list
          (save-excursion
            (goto-char (point-min))
            (loop with he-match-length = (length he-search-string)
                  while (search-forward-regexp +perl-package-regexp+ nil t)
                  when (and (> (length (match-string 0)) he-match-length)
                            (string= he-search-string
                                     (substring (match-string 0) 0 he-match-length)))
                  collect (match-string 0)))))
  (cond
   (he-expand-list
    (he-substitute-string (pop he-expand-list))
    t)
   (t
    (when old (he-reset-string))
    nil)))

(defun clear-frame-recursive-edit ()
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    (recursive-edit)))

(defun absorb-other-frame (arg)
  (interactive "p")
  (when (> 2 (length (visible-frame-list)))
    (error "Less than two visible frames"))
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left))
         (top  (frame-parameter frame 'top)))
    (other-frame arg)
    (delete-frame frame)
    (modify-frame-parameters nil `((left . ,left) (top . ,top)))))

;; Taken from post by Scott Frazer on gnu.emacs.help
(defun my-isearch-word ()
  "Surround current input with word/symbol delimiters and turn on regexp matching if necessary."
  (interactive)
  (unless isearch-regexp
    (isearch-toggle-regexp))
  (setq isearch-string (concat "\\_<" isearch-string "\\_>")
        isearch-message (mapconcat 'isearch-text-char-description isearch-string ""))
  (isearch-search-and-update))

(defun show-async-command (buffer-or-name process-name command &rest args)
  (let ((buffer (get-buffer-create buffer-or-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (setq mode-line-process '(":%s")))
    (display-buffer buffer)
    (apply #'start-process process-name buffer command args)))

(defun download (url)
  (let ((buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (unwind-protect
          (buffer-substring (or (search-forward "\n\n" nil t)
                                (error "Unable to locate downloaded data"))
                            (point-max))
        (kill-buffer buffer)))))

(defun insert-image-at-point ()
  (interactive)
  (let* ((url (thing-at-point-url-at-point))
         (inhibit-read-only t)
         (image (create-image (download url) nil t)))
    (save-excursion
      (move-beginning-of-line 2)
      (insert-image image)
      (insert "\n"))))

(defun curl-url-at-point ()
  (interactive)
  (format-shell-command "curl -s %s" (url-get-url-at-point)))

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp ">\\([ \\t]*\\)<" nil t)
      (replace-match "\n" nil nil nil 1))
    (indent-region begin end)))

(defun pretty-print-xml-buffer ()
  (interactive)
  (pretty-print-xml-region (point-min) (point-max)))

(defun fetch-my-feed (name)
  (interactive "sFeed name: ")
  (format-shell-command "curl -s http://seanmcafee.name/rss/%s.pl" name))

(defun dg ()
  "Delete from point to end of buffer, like Vim's dG command."
  (interactive)
  (kill-region (line-beginning-position) (point-max)))

(defun update-alist (alist &rest pairs)
  (append (copy-sequence pairs)
          (cl-delete-if (lambda (x) (cl-member (car x) pairs :key #'car)) alist)))

(defun show-twitpic ()
  (interactive)
  (let* ((url (or (url-get-url-at-point)
                  (error "No URL at point")))
         (buffer (generate-new-buffer "twitpic")))
    (with-current-buffer buffer
      (when (/= 0 (let ((coding-system-for-read 'no-conversion))
                    (call-process "twitpic.pl" nil t nil url)))
        (let ((message (buffer-string)))
          (kill-buffer buffer)
          (error "%s" message)))
      (image-mode)
      (pop-to-buffer buffer)
      (delete-other-windows))))

(defun perl-module-init ()
  (interactive)
  (let* ((case-fold-search nil)
         (bfn (or (buffer-file-name)
                  (error "Buffer not visiting a file")))
         (path (--take-while (string-match-p "\\`[[:upper:]]"
                                             (substring it 0 1))
                             (nreverse (split-string bfn "/" t)))))
    (insert
     "package "
     (s-join "::" (nreverse
                   (cons
                    (replace-regexp-in-string "\\.pm\\'" "" (first path) t t)
                    (rest path))))
     ";\n\n")
    (save-excursion (insert "use strict;\n\n\n\n1;\n"))))

(defun multi-occur-in-all-buffers (regexp &optional allbufs)
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers "" regexp))

;; An attempt at this Emacs SX question:
;; https://emacs.stackexchange.com/questions/10359/delete-portion-of-isearch-string-that-does-not-match-or-last-char-if-complete-m
(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  ;; Mostly copied from `isearch-del-char' and Drew's answer on the page above
  (interactive)
  (if (zerop (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (when isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(defun my-recompile ()
  (interactive)
  (aif (get-buffer "*compilation*")
      (progn
        (pop-to-buffer it)
        (recompile))
    (message "No old compilation buffer present")))

(defun multi-switch-buffers (prefix)
  "With no prefix argument, call ido-switch-buffer; otherwise call helm-buffers-list."
  (interactive "P")
  (if prefix (helm-buffers-list) (ido-switch-buffer)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
