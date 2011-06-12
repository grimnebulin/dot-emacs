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

(global-unset-key [(super w)])

(global-set-key [(super w) ?d] 'wrif-open-directory)
(global-set-key [(super w) ?p] 'wrif-play-directory)
(global-set-key [(super w) ?l] 'emms-playlist-mode-go)

(global-set-key [(super meta x)] 'emms-pause)
(global-set-key [(super meta p)] 'emms-pause)

(defconst wrif-skip-intervals '((7 . super) (60 . meta) (360 . control)))

(loop for (duration . modifier) in wrif-skip-intervals
  collecting modifier into modifiers do
  (loop for key in '(right left) for offset in `(,duration ,(- duration)) do
    (global-set-key (vector (append modifiers (list key)))
      `(lambda () (interactive) (emms-seek ,offset)))))

(defun wrif-seek-to (prefix)
  (interactive "p")
  (assert (<= 0 prefix))
  (let ((minutes (/ prefix 100))
        (seconds (mod prefix 100)))
    (emms-seek-to (+ seconds (* minutes 60)))))

(global-set-key [(super w) ?s] 'wrif-seek-to)
