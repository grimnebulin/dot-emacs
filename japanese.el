(defun insert-vowel-with-macron ()
  (interactive)
  (insert
   (case last-input-char
     (?a "ā")
     (?A "Ā")
     (?e "ē")
     (?E "Ē")
     (?i "ī")
     (?I "Ī")
     (?o "ō")
     (?O "Ō")
     (?u "ū")
     (?U "Ū")
     (otherwise (char-to-string last-input-char)))))

(loop for char in '(?a ?A ?e ?E ?i ?I ?o ?O ?u ?U) do
      (global-set-key (vector '(control x) ?9 char) 'insert-vowel-with-macron))


(defvar jdict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-f" 'jdict-strip-face-this-line)
    (define-key map "\C-c\C-o" 'jdict-open-page)
    (define-key map "\C-c\C-p" 'jdict-previous-page)
    (define-key map "\C-c\C-n" 'jdict-next-page)
    (define-key map "\C-c\C-t" 'jdict-toggle-page-size)
    (define-key map "\C-c\C-j" 'jdict-collapse-lines)
    ; (define-key map "\C-c." 'jdict-fix-punctuation)
    (define-key map "\M-j" 'jdict-romaji-to-hiragana)
    (define-key map "\M-k" 'jdict-consume-syllable)
    (define-key map "\C-c\C-c" 'jdict-tidy)
    ; (define-key map "\C-c\C-b" 'jdict-fix-parens)
    (define-key map "\C-c\C-h" 'jdict-header-line)
    (define-key map "\C-c\C-s" 'jdict-fix-sokuon)
    (define-key map "\C-c\C-a" 'jdict-fix-Js)
    (define-key map "\C-c\C-z" 'jdict-new-section)
    (define-key map "\C-c\C-i" 'jdict-fix-Is)
    (define-key map "\C-c\M-n" 'jdict-replace-n)
    (define-key map "\C-c'" 'jdict-insert-accented-vowel)
    (define-key map "\C-c\C-x" 'jdict-smart-new-section)
    (define-key map "\C-c\C-r" 'jdict-record-misread-characters)
    (define-key map "\C-c[" 'jdict-insert-opening-bracket)
    (define-key map "\C-c]" 'jdict-insert-closing-bracket)
    (define-key map "\C-c\C-l" 'jdict-new-subheading)
    (define-key map "\C-c(" 'jdict-fix-parens)
    (define-key map "\C-c." 'jdict-break-long-line)
    (macrolet
        ((scroll (dir &rest body)
            `(define-key map ,(vector (list 'shift dir))
               (lambda () (interactive)
                 (other-window 1) ,@body (other-window -1)))))
      (scroll right (image-forward-hscroll jdict-hscroll-distance))
      (scroll  left (image-forward-hscroll (- jdict-hscroll-distance)))
      (scroll    up (image-previous-line jdict-vscroll-distance))
      (scroll  down (image-next-line jdict-vscroll-distance)))
    map)
  "Keymap for JDict mode.")

(define-derived-mode jdict-mode text-mode "JDict"
  "Major mode for editing the Kodansha Learner's Dictionary.
\\{jdict-mode-map}")

(defun jdict-strip-face-this-line ()
  (interactive)
  (remove-list-of-text-properties
   (line-beginning-position)
   (line-end-position)
   '(face)))

(setq auto-hscroll-mode nil)

(defconst jdict-dictionary-dir "~/files/jdict-pages/")
(defconst jdict-hscroll-distance 5)
(defconst jdict-vscroll-distance 5)
(defconst jdict-large-page-suffix "60")
(defconst jdict-small-page-suffix "20")

(defvar jdict-current-page nil)
(defvar jdict-showing-large-page nil)
(defvar jdict-current-buffer nil)

(defun jdict-page-path (page)
  (format "%spage%03d-%s.png"
          jdict-dictionary-dir
          page
          (if jdict-showing-large-page
              jdict-large-page-suffix
            jdict-small-page-suffix)))

(defun jdict-open-page (arg)
  (interactive "p")
  (let ((path (jdict-page-path arg)))
    (unless (file-exists-p path)
      (error "No such path %s" path))
    (save-window-excursion
      (find-file-other-window path)
      (setq jdict-current-page arg)
      (setq jdict-current-buffer (current-buffer)))))

(defun jdict-next-page ()
  (interactive)
  (let ((previous-buffer jdict-current-buffer))
    (jdict-open-page (1+ jdict-current-page))
    (kill-buffer previous-buffer)))

(defun jdict-previous-page ()
  (interactive)
  (let ((previous-buffer jdict-current-buffer))
    (jdict-open-page (1- jdict-current-page))
    (kill-buffer previous-buffer)))

(defun jdict-toggle-page-size ()
  (interactive)
  (setq jdict-showing-large-page (not jdict-showing-large-page))
  (let ((previous-buffer jdict-current-buffer))
    (jdict-open-page jdict-current-page)
    (kill-buffer previous-buffer)))

(defun enlarge-japanese-text ()
  (interactive)
  (save-match-data
    (while (search-forward-regexp "^\\(\\cC\\|\\cH\\|\\cK\\|x\\)*\\cC\\(\\cC\\|\\cH\\|\\cK\\|x\\)*$" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face 'height2))))

(defconst hepburn-vowel-regexp
  "[aāäiīïuūüeēëoōö]")

(defconst hepburn-doubled-consonant-regexp
  "\\([bdfghkmnprstz]\\)\\1\\|tch")

(; defconst
 setq hepburn-basic-syllable-alist
  '(("a" . "あ") ("ā" . "ああ")
    ("i" . "い") ("ī" . "いい")
    ("u" . "う") ("ū" . "うう")
    ("e" . "え") ("ē" . "ええ")
    ("o" . "お") ("ō" . "おお")
    ("ka" . "か") ("kā" . "かあ")
    ("ki" . "き") ("kī" . "きい")
    ("ku" . "く") ("kū" . "くう")
    ("ke" . "け") ("kē" . "けえ")
    ("ko" . "こ") ("kō" . "こう")
    ("kya" . "きゃ") ("kyā" . "きゃあ")
    ("kyu" . "きゅ") ("kyū" . "きゅう")
    ("kyo" . "きょ") ("kyō" . "きょう")
    ("ga" . "が") ("gā" . "があ")
    ("gi" . "ぎ") ("gī" . "ぎい")
    ("gu" . "ぐ") ("gū" . "ぐう")
    ("ge" . "げ") ("gē" . "げえ")
    ("go" . "ご") ("gō" . "ごう")
    ("gya" . "ぎゃ") ("gyā" . "ぎゃあ")
    ("gyu" . "ぎゅ") ("gyū" . "ぎゅう")
    ("gyo" . "ぎょ") ("gyō" . "ぎょう")
    ("ta" . "た") ("tā" . "たあ")
    ("chi" . "ち") ("chī" . "ちい")
    ("tsu" . "つ") ("tsū" . "つう")
    ("te" . "て") ("tē" . "てえ")
    ("to" . "と") ("tō" . "とう")
    ("da" . "だ") ("dā" . "だあ")
    ("de" . "で") ("dē" . "でえ")
    ("do" . "ど") ("dō" . "どう")
    ("na" . "な") ("nā" . "なあ")
    ("ni" . "に") ("nī" . "にい")
    ("nu" . "ぬ") ("nū" . "ぬう")
    ("ne" . "ね") ("nē" . "ねえ")
    ("no" . "の") ("nō" . "のう")
    ("nya" . "にゃ") ("nyā" . "にゃあ")
    ("nyu" . "にゅ") ("nyū" . "にゅう")
    ("nyo" . "にょ") ("nyō" . "にょう")
    ("ha" . "は") ("hā" . "はあ")
    ("hi" . "ひ") ("hī" . "ひい")
    ("fu" . "ふ") ("fū" . "ふう")
    ("he" . "へ") ("hē" . "へえ")
    ("ho" . "ほ") ("hō" . "ほう")
    ("hya" . "ひゃ") ("hyā" . "ひゃあ")
    ("hyo" . "ひょ") ("hyō" . "ひょう")
    ("ba" . "ば") ("bā" . "ばあ")
    ("bi" . "び") ("bī" . "びい")
    ("bu" . "ぶ") ("bū" . "ぶう")
    ("be" . "べ") ("bē" . "べえ")
    ("bo" . "ぼ") ("bō" . "ぼう")
    ("bya" . "びゃ") ("byā" . "びゃあ")
    ("byu" . "びゅ") ("byū" . "びゅう")
    ("byo" . "びょ") ("byō" . "びょう")
    ("pa" . "ぱ") ("pā" . "ぱあ")
    ("pi" . "ぴ") ("pī" . "ぴい")
    ("pu" . "ぷ") ("pū" . "ぷう")
    ("pe" . "ぺ") ("pē" . "ぺえ")
    ("po" . "ぽ") ("pō" . "ぽう")
    ("pya" . "ぴゃ") ("pyā" . "ぴゃあ")
    ("pyu" . "ぴゅ") ("pyū" . "ぴゅう")
    ("pyo" . "ぴょ") ("pyō" . "ぴょう")
    ("ma" . "ま") ("mā" . "まあ")
    ("mi" . "み") ("mī" . "みい")
    ("mu" . "む") ("mū" . "むう")
    ("me" . "め") ("mē" . "めえ")
    ("mo" . "も") ("mō" . "もう")
    ("mya" . "みゃ") ("myā" . "みゃあ")
    ("myu" . "みゅ") ("myū" . "みゅう")
    ("myo" . "みょ") ("myō" . "みょう")
    ("ra" . "ら") ("rā" . "らあ")
    ("ri" . "り") ("rī" . "りい")
    ("ru" . "る") ("rū" . "るう")
    ("re" . "れ") ("rē" . "れえ")
    ("ro" . "ろ") ("rō" . "ろう")
    ("rya" . "りゃ") ("ryā" . "りゃあ")
    ("ryu" . "りゅ") ("ryū" . "りゅう")
    ("ryo" . "りょ") ("ryō" . "りょう")
    ("sa" . "さ") ("sā" . "さあ")
    ("sha" . "しゃ") ("shā" . "しゃあ")
    ("shi" . "し") ("shī" . "しい")
    ("sho" . "しょ") ("shō" . "しょう")
    ("shu" . "しゅ") ("shū" . "しゅう")
    ("su" . "す") ("sū" . "すう")
    ("se" . "せ") ("sē" . "せえ")
    ("so" . "そ") ("sō" . "そう")
    ("za" . "ざ") ("zā" . "ざあ")
    ("zu" . "ず") ("zū" . "ずう")
    ("ze" . "ぜ") ("zē" . "ぜえ")
    ("zo" . "ぞ") ("zō" . "ぞう")
    ("ja" . "じゃ") ("jā" . "じゃあ")
    ("ji" . "じ") ("jī" . "じい")
    ("ju" . "じゅ") ("jū" . "じゅう")
    ("je" . "じぇ") ("jē" . "じぇえ")
    ("jo" . "じょ") ("jō" . "じょう")
    ("cha" . "ちゃ") ("chā" . "ちゃあ")
    ("chu" . "ちゅ") ("chū" . "ちゅう")
    ("cho" . "ちょ") ("chō" . "ちょう")
    ("ya" . "や") ("yā" . "やあ")
    ("yu" . "ゆ") ("yū" . "ゆう")
    ("yo" . "よ") ("yō" . "よう")
    ("wa" . "わ") ("wā" . "わあ")
    ;; special parsing issues:
    ("tsii" . "つう")
    ("fii" . "ふう")
    ("shii" . "しゅう")
    ("gii" . "ぐう")
    ("sil" . "すう")
    ("nyii" . "にゅう")
    ("yii" . "ゆう")
    ("kyii" . "きゅう")
    ("0" . "を")))

(defconst hepburn-alternate-long-vowels
  '(("ā" "ä" "â" "ã")
    ("ē" "ë" "ê" "ẽ")
    ("ī" "ï" "î" "ĩ")
    ("ō" "ö" "ô" "õ")
    ("ū" "ü" "û" "ũ")))

(;defconst
 setq hepburn-syllable-alist
  (let ((expanded nil)
        (regexp (concat "[" (mapconcat 'car hepburn-alternate-long-vowels "") "]\\'")))
    (save-match-data
      (loop for pair in hepburn-basic-syllable-alist
            for (romaji . hiragana) = pair do
        (push pair expanded)
        (when (string-match regexp romaji)
          (let* ((vowels (cdr (assoc (match-string 0 romaji)
                                     hepburn-alternate-long-vowels))))
            (loop for vowel in vowels do
              (push (cons (replace-match vowel nil t romaji) hiragana)
                    expanded))))))
    expanded))
      
(;defconst
 setq hepburn-syllable-regexp
  (regexp-opt (mapcar 'car hepburn-syllable-alist)))

(;defconst
 setq hepburn-syllable-hash-table
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key . value) in hepburn-syllable-alist do
      (puthash key value table))
    table))

;; (defun jdict-romaji-to-hiragana ()
;;   (interactive)
;;   (save-match-data
;;     (let ((regexp (concat "\\=\\s-*\\(" hepburn-syllable-regexp "\\)"))
;;           (case-fold-search t))
;;       (unless (search-forward-regexp regexp nil t)
;;         (error "No romaji syllable following point."))
;;       (replace-match
;;        (gethash (downcase (match-string 1)) hepburn-syllable-hash-table))
;;       (cond
;;        ((search-forward-regexp "\\=n" nil t)
;;         (if (save-match-data (not (looking-at hepburn-vowel-regexp)))
;;             (replace-match "ん")
;;           (backward-char 1)))
;;        ((looking-at hepburn-doubled-consonant-regexp)
;;         (delete-char 1)
;;         (insert "っ"))))))

;; (defun jdict-consume-syllable ()
;;   (interactive)
;;   (save-match-data
;;     (let ((regexp (concat "\\=\\s-*\\(" hepburn-syllable-regexp "\\)"))
;;           (case-fold-search t))
;;       (unless (search-forward-regexp regexp nil t)
;;         (error "No romaji syllable following point."))
;;       (replace-match "")
;;       (cond
;;        ((search-forward-regexp "\\=n" nil t)
;;         (if (save-match-data (not (looking-at hepburn-vowel-regexp)))
;;             (replace-match "")))
;;        ((looking-at hepburn-doubled-consonant-regexp)
;;         (delete-char 1))))))

(defun jdict-romaji-to-hiragana (prefix)
  (interactive "p")
  (assert (and (wholenump prefix)))
  (let ((regexp (concat "\\=\\s *\\(" hepburn-syllable-regexp "\\)"))
        (case-fold-search t))
    (save-match-data
      (loop for char across (number-to-string prefix)
            for digit = (- char ?0)
            for delete = (= digit 0)
            for count = (max 1 digit) do
        (loop repeat count do
          (unless (search-forward-regexp regexp nil t)
            (error "Invalid Hepburn romanization"))
          (replace-match
           (if delete "" (gethash (downcase (match-string 1))
                                  hepburn-syllable-hash-table)))
          (cond
           ((search-forward-regexp "\\=n" nil t)
            (if (save-match-data (not (looking-at hepburn-vowel-regexp)))
                (replace-match (if delete "" "ん"))
              (unless delete (backward-char 1))))
           ((looking-at hepburn-doubled-consonant-regexp)
            (delete-char 1)
            (unless delete (insert "っ"))))
          finally (unless delete (insert "　")))
       finally (unless delete (backward-delete-char 1))))))

(defun jdict-consume-syllable ()
  (interactive)
  (jdict-romaji-to-hiragana 0))

(defconst jdict-misread-file "/Users/mcafee/quiz2/nihongo2/misread.el")

(defvar jdict-last-collapsed-characters)

(defvar jdict-last-collapse-point)

(defun jdict-collapse-lines ()
  (interactive)
  (let ((lbp (line-beginning-position 2))
        (lep (line-end-position 2)))
    (when (= lbp lep)
      (delete-region lbp (1+ lbp))))
  (setq jdict-last-collapsed-characters
    (save-match-data
      (when (looking-at ".*\n\\(\\cj\\{1,3\\}\\)$")
        (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
  (end-of-line)
  (setq jdict-last-collapse-point (point))
  (delete-region (line-end-position) (line-beginning-position 3)))

(defun jdict-read-misread-file ()
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file jdict-misread-file))
    (end-of-buffer)
    (preceding-sexp)))

(defun jdict-write-misread-file (alist)
  (with-temp-buffer
    (pprint alist (current-buffer) 20)
    (let ((coding-system-for-write 'utf-8))
      (write-region (point-min) (point-max) jdict-misread-file))))

(defun jdict-misread-characters-regexp (alist)
  (mapconcat (lambda (x) (regexp-quote (car x))) alist "\\|"))

(defconst jdict-correction-buffer "*JDict Corrections*")

(defun jdict-cdr-sort (a b)
  (< (cdr b) (cdr a)))

(defun jdict-correct-misread-characters ()
  (interactive)
  (save-window-excursion
    (set-window-buffer (prog1
                           (selected-window)
                         (split-window)
                         (other-window 1))
                       (get-buffer-create jdict-correction-buffer))
    (let* ((alist (jdict-read-misread-file))
           (case-fold-search nil)
           (regexp (concat "\\([a-z]*\\)\n\\(\\n*\\("
                           (jdict-misread-characters-regexp alist)
                           "\\)\\n*\\)\n\\([a-z]*\\)"))
           (done nil))
      (while (and (not done)
                  (progn (beginning-of-line) (re-search-forward regexp nil t)))
        (let* ((prefix (match-string 1))
               (prefix-length (length prefix))
               (suffix (match-string 4))
               (suffix-length (length suffix)))
          (when (and (< 0 prefix-length)
                     (< 0 suffix-length)
                     (<= 4 (+ prefix-length suffix-length)))
            (jdict-tidy-highlight
             (match-beginning (if (< 0 prefix-length) 1 3))
             (match-end (if (< 0 suffix-length) 4 3)))
            (let* ((replace-start
                    (1- (match-beginning 2)))
                   (replace-end
                    (1+ (match-end 2)))
                   (pair
                    (assoc (match-string 3) alist))
                   (corrections
                    (setcdr pair (sort (cdr pair) 'jdict-cdr-sort)))
                   (choices
                    (loop for c in corrections
                          for i from 1
                          collect (cons (format "%s" i) (car c)))))
              (with-current-buffer jdict-correction-buffer
                (erase-buffer)
                (loop for choice in choices do
                      (insert (car choice) "." prefix (cdr choice) suffix " "))
                (let ((fill-column (- (window-width) 15)))
                  (fill-region (point-min) (point-max)))
                (goto-char (point-min))
                (while (search-forward "." nil t)
                  (replace-match ". "))
                (fit-window-to-buffer (get-buffer-window jdict-correction-buffer)))
              (recenter 7)
              (flet ((bump (str)
                       (delete-region replace-start replace-end)
                       (goto-char replace-start)
                       (insert str)
                       (alist-increment corrections str)
                       (jdict-write-misread-file alist)
                       (setq done2 t)))
                (loop with done2 = nil until done2 do
                  (let* ((response (read-string "Replace with: "))
                         (pair2 (assoc response choices)))
                    (cond
                     (pair2
                      (bump (cdr pair2)))
                     ((string-match "\\`[a-z]+\\'" response)
                      (bump response))
                     ((string= "Q" response)
                      (setq done2 t done t))
                     ((string= "" response)
                      (setq done2 t))
                     (t
                      (message "Invalid response")
                      (beep)
                      (sleep-for 0 500)))))))
            (jdict-tidy-dehighlight)))))))

(defun jdict-record-misread-characters ()
  (interactive)
  (unless jdict-last-collapsed-characters
    (error "No collapsed characters"))
  (when (= (point) jdict-last-collapse-point)
    (error "Too few characters"))
  (when (<= 10 (abs (- (point) jdict-last-collapse-point)))
    (error "Too many characters"))
  (let* ((str (buffer-substring-no-properties
               (point) jdict-last-collapse-point))
         (alist (jdict-read-misread-file))
         (pair (assoc jdict-last-collapsed-characters alist)))
    (unless pair
      (setq pair (cons jdict-last-collapsed-characters nil)
            alist (cons pair alist)))
    (let ((pair2 (assoc str (cdr pair))))
      (if pair2
          (setcdr pair2 (1+ (cdr pair2)))
        (setf pair2 (cons str 1)
              (cdr pair) (cons pair2 (cdr pair)))))
    (jdict-write-misread-file alist)))

(defun alist-increment (alist key)
  (let ((pair (assoc key alist)))
    (if pair
        (incf (cdr pair))
      (setcdr (last alist) (list (cons key 1))))))

(defun jdict-insert-opening-bracket ()
  (interactive)
  (insert "【"))

(defun jdict-insert-closing-bracket ()
  (interactive)
  (insert "】"))

(defun jdict-new-subheading (prefix str)
  (interactive "P\nsKill string? ")
  (save-match-data
    (let ((case-fold-search nil)
          (regexp (concat "\\s-*" (regexp-quote str) "\\s-*")))
      (if (not (re-search-forward regexp (line-end-position) t))
          (error "Not found")
        (insert "\n* ")
        (delete-region
         (if prefix (line-beginning-position 0) (match-beginning 0))
         (match-end 0))))))

;; (defun jdict-fix-parens ()
;;   (interactive)
;;   (save-match-data
;;     (save-excursion
;;       (goto-char (line-beginning-position))
;;       (while (search-forward-regexp "\\b\\([[(]\\)"
;;                                     (line-end-position) t)
;;         (replace-match " \\1"))
;;       (goto-char (line-beginning-position))
;;       (while (search-forward-regexp "\\([])]\\)\\b"
;;                                     (line-end-position) t)
;;         (replace-match "\\1 ")))))

(defun jdict-fix-parens ()
  (interactive)
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (while (re-search-forward "\\([[:alpha:]]\\) (" (line-end-position) t)
        (replace-match (concat (match-string 1) "(")))
      (beginning-of-line)
      (while (search-forward "}" (line-end-position) t)
        (replace-match ")")))))

(defun jdict-break-long-line ()
  (interactive)
  (let ((fill-column (- (window-width) 5)))
    (fill-region (line-beginning-position) (line-end-position))))

(defun jdict-header-line (arg)
  (interactive "P")
  (when arg
    (let ((char (buffer-substring-no-properties (point) (1+ (point)))))
      (jump-to-register ?j)
      (insert char)))
  (jdict-strip-face-this-line)
  (beginning-of-line)
  (insert "*** ")
  (end-of-line)
  (insert " - "))

(defun jdict-fix-sokuon ()
  (interactive)
  (save-match-data
    (let ((sokuon (cond ((looking-at "つ") "っ")
                        ((looking-at "ツ") "ッ")
                        (t (error "Point is not on a \"つ\" or \"ツ\"")))))
    (insert (apply 'propertize sokuon (text-properties-at (point))))
    (delete-char 1)
    (backward-char 1))))

(defun jdict-fix-Js ()
  (interactive)
  (save-match-data
    (beginning-of-line)
    (while (search-forward-regexp "\\BJ\\B" (line-end-position) t)
      (replace-match "l" t))))

(defun jdict-new-section ()
  (interactive)
  (save-match-data
    (search-forward "【" (line-end-position))
    (backward-char 1)
    (insert "\n")
    (insert "* ")
    (delete-indentation 1)
    (delete-indentation 1)
    (jdict-strip-face-this-line)))

(defun jdict-fix-Is ()
  (interactive)
  (save-match-data
    (while (search-forward "í" (line-end-position) t)
      (replace-match "i"))))

(defun jdict-replace-n ()
  (interactive)
  (save-match-data
    (search-forward-regexp "\\=n")
    (replace-match "ん")))

(defun jdict-insert-accented-vowel (char)
  (interactive "cAccent what vowel? ")
  (insert
   (case char
     (?o "ō")
     (?O "Ō")
     (?u "ū")
     (?U "Ū")
     (otherwise (error "Can only accent capital or lowercase O or U")))))

(;defconst
 setq jdict-section-marker '(("f[})]" . "2") ("?Ð" . "2") ("o" . "3") ("【" . " 【") ("•" . "") ("8" . "2")))

(defun jdict-smart-new-section ()
  (interactive)
  (save-match-data
    (search-forward-regexp "\\s-*\\(Ð\\|f[})]\\|\\bo\\b\\|\\b8\\b\\|【\\|•\\)\\s-*" (line-end-position))
    (replace-match (concat "\n\n*" (cdr (assoc (match-string 1) jdict-section-marker)) " "))))

;;;;;

(defun jdict-tidy ()
  (interactive)
  (setq jdict-tidy-saved-case-fold-search case-fold-search
        case-fold-search nil
        jdict-tidy-pos (point)
        jdict-tidy-last-type nil
        overriding-terminal-local-map jdict-tidy-mode-map))

(defvar jdict-tidy-mode-map
  (let* ((map (make-keymap))
         (table (nth 1 map)))
;;     (loop for char in (generic-character-list) do
;;       (set-char-table-default table char 'jdict-tidy-printing-char))
;;     (define-key map [t] 'jdict-tidy-other-control-char)
    (loop for char below ?\s do
      (define-key map (make-string 1 char) 'jdict-tidy-other-control-char))
    (loop for (first last) in '((?A ?Z) (?a ?z)) do
      (loop for char from first to last do
        (define-key map (vector char) 'jdict-tidy-letter)))
;;     (loop for digit from ?0 to ?9 do
;;       (define-key map (vector digit) 'digit-argument))
;;     (define-key map "\C-u" 'universal-argument)
    ;; skipping "To handle local keybindings with meta char prefix keys" for now
;;     (define-key map " " 'jdict-tidy-confirm-space)
    (define-key map "\C-m" 'jdict-tidy-confirm)
    (define-key map "\C-i" 'jdict-tidy-I-to-l)
    (define-key map "\C-l" 'jdict-tidy-l-to-bracket)
    (define-key map "\C-g" 'jdict-tidy-abort)
    (define-key map "\C-j" 'jdict-tidy-done)
;;     (define-key map "\C-i" 'jdict-tidy-fix-capital-I)
;;     (define-key map "\C-l" 'jdict-tidy-fix-lowercase-l)
;;     (define-key map "\C-e" 'jdict-tidy-skip-to-end-of-line)
    map))

;; (defun jdict-tidy-skip-to-end-of-line ()
;;   (interactive)
;;   (jdict-tidy-move)
;;   (jdict-tidy-done)
;;   (end-of-line))

(defun jdict-tidy-letter ()
  (interactive)
  (let* ((char (char-to-string last-command-char)))
    (jdict-tidy-move char 'jdict-tidy-handle-letter 'letter)))

(defun jdict-tidy-handle-letter (match)
  (insert " ")
  (jdict-tidy-highlight jdict-tidy-pos (1- (point))))

(defun jdict-tidy-I-to-l ()
  (interactive)
  (jdict-tidy-move "I" "l" 'I))

(defun jdict-tidy-l-to-bracket ()
  (interactive)
  (jdict-tidy-move "l" "]" 'l))

(defun jdict-tidy-undo ()
  (case jdict-tidy-last-type
    (letter
     (backward-delete-char 1))
    (I
     (backward-delete-char 1)
     (insert "I"))
    (l
     (backward-delete-char 1)
     (insert "l"))))

(defun jdict-tidy-other-control-char ()
  (interactive)
  (setq unread-command-events
        (append (listify-key-sequence (this-command-keys)) unread-command-events))
  (jdict-tidy-done))

(defun jdict-tidy-move (regexp replacement type)
  (when (and type jdict-tidy-last-type (eq type jdict-tidy-last-type))
    (jdict-tidy-undo))
  (save-match-data
    (let ((full-regexp (concat "\\(\\<\\)\\|\\(" regexp "\\)"))
          (found nil)
          (done nil)
          match)
      (while (not (or done found))
         (if (not (search-forward-regexp full-regexp (line-end-position) t))
            (setq done t)
          (setq match (match-string 2))
          (when (match-string 1)
            (setq jdict-tidy-pos (point))
            (if (not (looking-at regexp))
                (forward-char 1)
              (setq match (match-string 0))
              (forward-char (length match))))
          (cond
           ((not (setq found match)))
           ((stringp replacement)
            (replace-match replacement t)
            (jdict-tidy-highlight (- (point) (length replacement)) (point)))
           (t
            (funcall replacement match)))))))
  (setq jdict-tidy-last-type type))

;; (defun jdict-tidy-letter ()
;;   (interactive)
;;   (when jdict-tidy-spaced
;;     (backward-delete-char 1)
;;     (setq jdict-tidy-spaced nil))
;;   (save-match-data
;;     (let* ((char (char-to-string last-command-char))
;;            (regexp (concat "\\s-*\\([.,;]\\)\\s-*\\|\\<\\(.\\)\\|" (regexp-quote char)))
;;            (found nil)
;;            (done nil))
;;       (while (not (or done found))
;;         (if (not (search-forward-regexp regexp (line-end-position) t))
;;             (setq done t)
;;           (if (match-string 1)
;;               (replace-match (concat (match-string 1) " "))
;;             (setq found (or (not (match-string 2))
;;                             (progn (setq jdict-tidy-pos (1- (point)))
;;                                    (string= (match-string 2) char)))))))
;;       (when found
;;         (insert " ")
;;         (setq jdict-tidy-spaced t)
;;         (jdict-tidy-highlight jdict-tidy-pos (1- (point)))
;;         (jdict-tidy-msg "Jdict-Tidy: %s" (buffer-substring jdict-tidy-pos (point)))))))

;; (defun jdict-tidy-confirm-space ()
;;   (interactive)
;;   (setq jdict-tidy-spaced nil)
;;   (jdict-tidy-dehighlight))

(defun jdict-tidy-confirm ()
  (interactive)
  (jdict-tidy-dehighlight)
  (setq jdict-tidy-last-type nil))

(defun jdict-tidy-abort ()
  (interactive)
  (jdict-tidy-done))

(defun jdict-tidy-done ()
  (interactive)
  (jdict-tidy-dehighlight)
  (setq case-fold-search jdict-tidy-saved-case-fold-search)
  (setq overriding-terminal-local-map nil))

(defun jdict-tidy-msg (str &rest args)
  (let ((message-log-max nil))
    (apply 'message str args)))

(setq jdict-tidy-overlay nil)

(defun jdict-tidy-highlight (beg end)
  (if jdict-tidy-overlay
      (move-overlay jdict-tidy-overlay beg end)
    (setq jdict-tidy-overlay (make-overlay beg end))
    (overlay-put jdict-tidy-overlay 'priority 1001)
    (overlay-put jdict-tidy-overlay 'face 'isearch)))

(defun jdict-tidy-dehighlight ()
  (when jdict-tidy-overlay
    (delete-overlay jdict-tidy-overlay)
    (setq jdict-tidy-overlay nil)))

(setq h2k '(
("あ" . "ア")
("い" . "イ")
("う" . "ウ")
("え" . "エ")
("お" . "オ")
("か" . "カ")
("き" . "キ")
("く" . "ク")
("け" . "ケ")
("こ" . "コ")
("ゃ" . "ャ")
("ゅ" . "ュ")
("ょ" . "ョ")
("が" . "ガ")
("ぎ" . "グ")
("ぐ" . "グ")
("げ" . "ゲ")
("ご" . "ゴ")
("さ" . "サ")
("し" . "シ")
("す" . "ス")
("せ" . "セ")
("そ" . "ソ")
("ざ" . "ザ")
("じ" . "ジ")
("ず" . "ズ")
("ぜ" . "ゼ")
("ぞ" . "ゾ")
("た" . "タ")
("ち" . "チ")
("つ" . "ツ")
("て" . "テ")
("と" . "ト")
("だ" . "ダ")
("で" . "デ")
("ど" . "ド")
("な" . "ナ")
("に" . "ニ")
("ぬ" . "ヌ")
("ね" . "ネ")
("の" . "ノ")
("は" . "ハ")
("ひ" . "ヒ")
("ふ" . "フ")
("へ" . "ヘ")
("ほ" . "ホ")
("ば" . "バ")
("び" . "ビ")
("ぶ" . "ブ")
("べ" . "ベ")
("ぼ" . "ボ")
("ぱ" . "パ")
("ぴ" . "ピ")
("ぷ" . "プ")
("ぺ" . "ペ")
("ぽ" . "ポ")
("ま" . "マ")
("み" . "ミ")
("む" . "ム")
("め" . "メ")
("も" . "モ")
("や" . "ヤ")
("ゆ" . "ユ")
("よ" . "ヨ")
("ら" . "ラ")
("り" . "リ")
("る" . "ル")
("れ" . "レ")
("ろ" . "ロ")
("わ" . "ワ")))