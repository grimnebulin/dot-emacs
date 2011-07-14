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

(macrolet ((set-seek (delta &rest keys)
              `(global-set-key [,keys]
                  (lambda () (interactive) (emms-seek ,delta)))))
  (set-seek   +7 super right)
  (set-seek   -7 super left)
  (set-seek  +60 super meta right)
  (set-seek  -60 super meta left)
  (set-seek +360 super meta control right)
  (set-seek -360 super meta control left))

(defun wrif-seek-to (prefix)
  (interactive "p")
  (assert (<= 0 prefix))
  (let ((minutes (/ prefix 100))
        (seconds (mod prefix 100)))
    (emms-seek-to (+ seconds (* minutes 60)))))

(global-set-key [(super w) ?s] 'wrif-seek-to)
