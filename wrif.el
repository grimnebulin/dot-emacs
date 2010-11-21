;; WRIF:

(defconst wrif-directory "~/WRIF")

(defun wrif-open-directory ()
  (interactive)
  (dired wrif-directory)
  (revert-buffer))

(defun wrif-play-directory ()
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (assert (= 1 (length file-list)))
    (emms-play-directory (car file-list)))
  (kill-buffer nil)
  (emms-playlist-mode-go)
  (goto-line 2))

(global-set-key [(super w) ?d] 'wrif-open-directory)
(global-set-key [(super w) ?p] 'wrif-play-directory)
(global-set-key [(super w) ?l] 'emms-playlist-mode-go)

(global-set-key [(super meta x)] 'emms-pause)
(global-set-key [(super meta p)] 'emms-pause)

(defconst wrif-skip-intervals '((7 . hyper) (60 . meta) (360 . control)))

(loop with modifiers = nil
      for (magnitude . modifier) in wrif-skip-intervals do
        (setq modifiers (append modifiers (list modifier)))
        (loop for (key . func) in '((right . +) (left . -)) do
          (global-set-key (vector (append modifiers (list key)))
                          `(lambda () (interactive)
                             (emms-seek ,(funcall func magnitude))))))
