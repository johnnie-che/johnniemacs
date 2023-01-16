(provide 'timefile)

;;---------
;; Syntax Highlighting
;;---------
(define-generic-mode 'timefile-mode
  '("#")
  '("tracked")
  '(("^.+:" . 'font-lock-variable-name-face)
    ("[0-9]+m" . 'font-lock-constant-face)
    ("[0-9]+h" . 'font-lock-constant-face))
  '(".time\\'")
  nil
  "Generic mode for .time files.")

;;--------
;; Time operations
;;--------

;; Const
(setq interval-hours-format "[0-9]\\{1,2\\}")
(setq interval-minutes-format "[0-9]\\{2\\}")
(setq time-minutes-format "[0-9]\\{1,3\\}")

;; Utility
(defun jtracker-replace (fn)
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(funcall fn))))
  nil)

;; Convert interval
(defun jtracker-calc-time ()
  "Calc minutes"
  (interactive "*")
  (jtracker-replace 'jtracker-replace-intervals)
  nil)

(defun jtracker-replace-intervals ()
  (while (re-search-forward "[0-9]\\{1,2\\}.[0-9]\\{2\\}-[0-9]\\{1,2\\}.[0-9]\\{2\\}" nil t)
    (let ((interval (match-string 0)))
      (delete-region (match-beginning 0) (point))
      (goto-char (match-beginning 0))
      (insert
       (jtracker-convert-interval interval)))))

(defun jtracker-parse-interval (interval)
  "Parse time interval and returns hours and minutes."
  (string-match
   (concat
    "\\(" interval-hours-format "\\)"
    "."
    "\\(" interval-minutes-format "\\)"
    "-"
    "\\(" interval-hours-format "\\)"
    "."
    "\\(" interval-minutes-format "\\)")
   interval)
  (list (match-string 1 interval)
   (match-string 2 interval)
   (match-string 3 interval)
   (match-string 4 interval)))
   
(defun jtracker-convert-interval (interval)
  "Convert time interval into minutes"
  (concat
   (number-to-string
  (cl-multiple-value-bind
      (h1 m1 h2 m2)
      (mapcar #'string-to-number
	      (jtracker-parse-interval interval))
    (jtracker-calc-minutes h1 m1 h2 m2)))
  "m"))

;; should be private ???
(defun jtracker-calc-minutes (h1 m1 h2 m2)
  (- (+ (* 60 h2) m2) (* 60 h1) m1))

;; Sum minutes
(defun jtracker-sum-minutes ()
  "Sum minutes"
  (interactive "*")
  (jtracker-replace 'jtracker-replace-minutes)
  nil)

(defun jtracker-replace-minutes ()
  (while (re-search-forward (concat
			     time-minutes-format
			     "m "
			     time-minutes-format
			     "m") nil t)
    (let ((minutes (match-string 0)))
      (delete-region (match-beginning 0) (point))
      (goto-char (match-beginning 0))
      (insert (jtracker-convert-minutes minutes)))))

(defun jtracker-parse-minutes (minutes)
  (string-match
   (concat
    "\\(" time-minutes-format "\\)"
    "m "
    "\\(" time-minutes-format "\\)"
    "m")
   minutes)
  (list
   (match-string 1 minutes)
   (match-string 2 minutes)))

(defun jtracker-convert-minutes (minutes)
  (concat
   (number-to-string
    (cl-multiple-value-bind
	(m1 m2)
	(mapcar #'string-to-number
		(jtracker-parse-minutes minutes))
      (+ m1 m2)))
   "m"))

;; Additional
(defun jtracker-now ()
  (interactive "*")
  (insert
   (format-time-string
    "%H.%M"
    (*
     (round
      (/
       (float
	(time-convert (current-time) 'integer))
       300))
     300))))

(defun jtracker-date ()
  (interactive "*")
  (insert
   (format-time-string "-[%d.%m]-")))

;;----------
;; Hotkeys
;;----------
(define-key ctl-x-map (kbd "j n") 'jtracker-now)
(define-key ctl-x-map (kbd "j c") 'jtracker-calc-time)
(define-key ctl-x-map (kbd "j s") 'jtracker-sum-minutes)
(define-key ctl-x-map (kbd "j d") 'jtracker-date)
