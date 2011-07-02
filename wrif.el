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

(global-unset-key [(super w)])
(global-set-key [(super w) ?d] 'wrif-open-directory)
(global-set-key [(super w) ?p] 'wrif-play-directory)
(global-set-key [(super w) ?l] 'emms-playlist-mode-go)

(global-set-key [(super meta x)] 'emms-pause)
(global-set-key [(super meta p)] 'emms-pause)
(global-set-key [pause] 'emms-pause)

(defconst wrif-skip-intervals '((7 . super) (60 . meta) (360 . control)))

(loop for (magnitude . modifier) in wrif-skip-intervals
      collecting modifier into modifiers
      do (loop for key in '(right left)
               and delta = magnitude then (- magnitude)
               do (global-set-key (vector (append modifiers (list key)))
                                  `(lambda () (interactive) (emms-seek ,delta)))))

;; (defconst wrif-skip-intervals '(360 60 7 super meta control))

;; (loop for magnitude in wrif-skip-intervals
;;       and modifiers on (cdddr wrif-skip-intervals)
;;       do (loop for key in '(right left)
;;                and delta = magnitude then (- magnitude)
;;                do (global-set-key (vector (append modifiers (list key)))
;;                                   `(lambda () (interactive) (emms-seek ,delta)))))

