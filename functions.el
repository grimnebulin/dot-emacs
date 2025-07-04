;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(require 'ido)

(defun princ-to-string (x)
  (format "%s" x))

;;

(defun other-window-delete-rest (kill-current)
  "Goes to the next window with other-window, then makes that window
the only visible window.  With a prefix argument, also kills the
starting buffer."
  (interactive "P")
  (if (>= 1 (length (window-list)))
      (error "No other window")
    (let ((buffer (current-buffer)))
      (other-window 1)
      (delete-other-windows)
      (when kill-current
        (kill-buffer buffer)))))

;;

(defun perlmod (module)
  "Reads the name of a Perl module, then loads the file that implements
that module, if it exists."
  (interactive "sPerl module: ")
  (let ((path (perl-module-path module)))
    (find-file path)
    (message "%s" path)))

(defun perl-module-path (module)
  "Returns the path to the Perl file that implements the given module."
  (with-temp-buffer
    (let* ((path (concat (replace-regexp-in-string "::" "/" module) ".pm"))
           (status (call-process "perl" nil t nil (concat "-M" module) "-e" "print $INC{ shift() }" path))
           (output (buffer-string)))
      (if (zerop status)
          output
        (error "%s" output)))))

(defun pymod (module)
  "Reads the name of a Python module, then loads the file that implements
that module, if it exists."
  (interactive "sPython module: ")
  (let ((path (python-module-path module)))
    (find-file path)
    (message "%s" path)))

(defun python-module-path (module)
  "Returns the path to the Python file that implements the given module."
  (with-temp-buffer
    (let* ((status (call-process "python" nil t nil "-c" "
import importlib, sys
try:
 sys.stdout.write(importlib.import_module(sys.argv[1]).__file__)
except ImportError, e:
 sys.stderr.write(e.message)
 sys.exit(1)
" module))
           (output (buffer-string)))
      (if (zerop status)
          (replace-regexp-in-string (rx ".pyc" eos) ".py" output)
        (error "%s" output)))))

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

(defun stackoverflow-copy-code-snippet (begin end)
  (interactive "r")
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) 4)
      (clipboard-kill-ring-save (point-min) (point-max))
      (message "Code snippet copied to system clipboard"))))

(defun kill-this-buffer-and-associated-file ()
  (interactive)
  (let ((file-name (or (buffer-file-name) (error "Not visiting a file"))))
    (when (kill-buffer)
      (delete-file file-name))))

(defun upcase-region-or-characters (arg)
  (interactive "p")
  (upcase-region (point) (if mark-active (mark) (+ arg (point)))))

(defun downcase-region-or-characters (arg)
  (interactive "p")
  (downcase-region (point) (if mark-active (mark) (+ arg (point)))))

(defun is-interactive-shell-buffer (buffer)
  (and (eq 'shell-mode (buffer-local-value 'major-mode buffer))
       (let ((proc (get-buffer-process buffer)))
         (and proc (not (process-sentinel proc))))))

(defun crontab ()
  (interactive)
  (shell-command "EDITOR=emacsclient crontab -e &"))

(defun toggle-case ()
  (interactive)
  (let ((case-fold-search nil))
    (when (search-forward-regexp (rx point (| (group upper) (group lower))) nil t)
      (replace-match
       (if (match-string 1)
           (downcase (match-string 1))
         (upcase (match-string 2)))
       t)
      (backward-char 1))))

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
	(rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

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
	 (temp-name (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename temp-name 1)
      (delete-file filename)
      (set-visited-file-name temp-name)
      (set-buffer-modified-p nil)
      t)))

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
  (when (kill-buffer)
    (delete-frame)))

(require 'thingatpt)

(defun shell-in-directory (dir &optional suffix)
  (setq dir (file-name-directory dir))
  (or (loop for b being the buffers
            if (and (is-interactive-shell-buffer b)
                    (string= dir (buffer-local-value 'default-directory b)))
            return (switch-to-buffer b))
      (let ((default-directory dir))
        (shell
         (generate-new-buffer-name
          (if suffix (format "*shell-%s*" suffix) "*shell*")))
        (delete-other-windows))))

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

(defun multi-occur-in-all-buffers (regexp &optional allbufs)
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers "" regexp allbufs))

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

(defun multi-switch-buffers (prefix)
  "With no prefix argument, call ido-switch-buffer; otherwise call helm-buffers-list."
  (interactive "P")
  (if prefix (helm-buffers-list) (ido-switch-buffer)))

(defun count-region-bytes (start end)
  (interactive "r")
  (message "Region contains %d bytes" (string-bytes (buffer-substring-no-properties start end))))

(defun isearch-yank-symbol ()
  (interactive)
  (isearch-yank-internal (lambda () (skip-syntax-forward "w_") (point))))

(defun my-dired-jump (prefix)
  (interactive "P")
  (let* ((buffer (current-buffer))
         (name (buffer-name buffer)))
    (dired-jump)
    (when (and prefix (not (string= name (buffer-name (current-buffer)))))
      (when (kill-buffer buffer)
        (message "Killed buffer %s" name)))))

(defmacro with-synchronous-download (url &rest body)
  (declare (indent 1))
  (let ((buffer (make-symbol "buffer"))
        (urlsym (make-symbol "url")))
    `(let* ((,urlsym ,url)
            (,buffer (url-retrieve-synchronously ,urlsym t t)))
       (unwind-protect
           (with-current-buffer ,buffer
             (goto-char (point-min))
             (unless (search-forward "\n\n" nil t)
               (error "Download of %s failed: %s" ,urlsym (buffer-string)))
             ,@body)
         (kill-buffer ,buffer)))))

(defun url-hexify-region (start end)
  (interactive "r")
  (kill-region start end)
  (insert (url-hexify-string (car kill-ring))))

(defun isearch-or-swiper ()
  (interactive)
  (if current-prefix-arg (swiper) (isearch-forward nil t)))

(require 'url)

(defun yank-url-sans-query ()
  (interactive)
  (let ((url (url-generic-parse-url (current-kill 0))))
    (cl-callf (lambda (x) (replace-regexp-in-string (rx "?" (* anything)) "" x)) (url-filename url))
    (insert (url-recreate-url url))))

(defun json-pretty-print-region-or-element-at-point (unpretty)
  (interactive "P")
  (save-excursion
    (if (use-region-p)
        (json-pretty-print (region-beginning) (region-end) unpretty)
      (json-pretty-print (point) (progn (forward-sexp) (point)) unpretty))))

(defvar json-pretty-print-array-on-single-line-predicate (lambda (array) (loop for x across array always (numberp x))))

(with-eval-after-load 'json
  (defun encode-json-array-of-numbers-on-one-line (encode array)
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print (not (funcall json-pretty-print-array-on-single-line-predicate array))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array))))

(defun json-array-length ()
  (interactive)
  (if (looking-at (rx "["))
      (save-excursion
        (message "%d" (length (json-read))))
    (error "Point is not on a JSON array")))

(defun maybe-ignore-ido (func &rest args)
  (if (loop for command = this-command then (symbol-function command)
            while (symbolp command)
            thereis (eq 'ignore (get command 'ido)))
      (let ((read-buffer-function nil))
        (run-hook-with-args 'ido-before-fallback-functions 'read-buffer)
        (apply #'read-buffer args))
    (apply func args)))

(defun vim-style-open-line (prefix)
  (interactive "P")
  (beginning-of-line)
  (open-line 1)
  (unless (equal prefix '(4))
    (indent-according-to-mode)))

(defun push-mark-backward-up-list ()
  (interactive)
  (push-mark)
  (call-interactively 'backward-up-list))

(defun push-mark-paredit-backward-up ()
  (interactive)
  (push-mark)
  (call-interactively 'paredit-backward-up))

(defun copy-rectangle-as-single-line (start end separator)
  (interactive
   (list
    (region-beginning)
    (region-end)
    (if current-prefix-arg (read-string "Line separator: ") ", ")))
  (unless (use-region-p)
    (error "Region is not active"))
  (let (killed-rectangle)
    (copy-rectangle-as-kill start end)
    (kill-new (s-join separator killed-rectangle))))

(defun notify-of-remote-shell-command (args)
  (save-match-data
    (cons
     (if (string-match (rx bos "/ssh:" (group (1+ (not (any ?:))))) default-directory)
         (format "REMOTE shell command on %s: " (match-string 1 default-directory))
       (car args))
     (cdr args))))

(defun helm-unicode-with-kill-option ()
  (interactive)
  (let ((source (helm-unicode-source)))
    (callf append (alist-get 'action source) `(("Kill Character" . ,(lambda (candidate) (kill-new (substring candidate -1))))))
    (helm :sources source :buffer "*helm-unicode-search*")))

(defun unicode-italicize-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (search-forward-regexp "[A-Za-z]" end t)
      (replace-match
       (string
        (pcase (aref (match-string 0) 0)
          ((and (pred (lambda (x) (<= ?A x ?Z))) upper)
           (+ upper (- ?𝐴 ?A)))
          (?h ?ℎ)
          (lower
           (+ lower (- ?𝑎 ?a)))))))))

(defun add-process-sentinel (process sentinel)
  (set-process-sentinel process
                        (if-let (old-sentinel (process-sentinel process))
                            (lambda (&rest args)
                              (apply old-sentinel args)
                              (apply sentinel args))
                          sentinel)))

(defun other-window-dwim ()
  (interactive)
  (if (cdr (window-list))
      (other-window 1)
    (other-frame 1)))

(defun my-kill-whole-line (arg)
  (interactive "p")
  (forward-line 0)
  (kill-whole-line arg))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
