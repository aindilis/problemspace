
;;; problemspace.el --- ProblemSpace-like FRDCSA/UniLang world-state symbolic-VR HCI

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: Jason Sayne <jasayne@frdcsa.org>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;; This visualizes the UniLang event stream in the style from the film
;; the ProblemSpace

;;; Code:

;; start writing Sun Feb 13 02:15:02 EST 2005

(require 'timer)
(require 'tabify)
(eval-when-compile (require 'cl))

(defvar problemspace-idle 20
 "*Seconds to idle before starting ProblemSpace.")

;; Vector of functions that problemspace out.  `problemspace' will execute one of
;; these functions, randomly chosen.  The chosen function is invoked
;; in the *problemspace* buffer, which contains the text of the selected
;; window.  If the function loops, it *must* periodically check and
;; halt if `input-pending-p' is t (because quitting is disabled when
;; Emacs idle timers are run).

(defvar problemspace-programs [
			       problemspace-display-matrix
			       ])

(defmacro problemspace-orig (&rest body)
 `(with-current-buffer (get 'problemspace 'orig-buffer)
   ,@body))

;;;###autoload
(defun problemspace ()
 "ProblemSpace out, completely."
 (interactive)
 (problemspace-set-colors)
 (find-file "/var/lib/myfrdcsa/codebases/internal/problemspace/data/matrixdata")
 (delete-other-windows)
 (let ((timer (get 'problemspace 'timer)))
  (and (timerp timer) (cancel-timer timer)))
 (put 'problemspace 'timer nil)
 (let ((f (selected-frame))
       (outbuf (get-buffer-create "*problemspace*"))
       (text (buffer-substring (window-start) (window-end)))
       (wp (1+ (- (window-point (selected-window))
		(window-start)))))
  (put 'problemspace 'orig-buffer (current-buffer))
  (set-buffer outbuf)
  (setq mode-name "ProblemSpace")
  (erase-buffer)
  (insert text)
  (switch-to-buffer outbuf)
  (setq buffer-undo-list t)
  (untabify (point-min) (point-max))
  (set-window-start (selected-window) (point-min))
  (set-window-point (selected-window) wp)
  (sit-for 0 500)
  (let ((pgm (elt problemspace-programs (random (length problemspace-programs))))
	(ct (and f (frame-parameter f 'cursor-type))))
   (when ct (modify-frame-parameters f '((cursor-type . (bar . 0)))))
   (condition-case nil
    (progn
     (message "Zoning... (%s)" pgm)
     (garbage-collect)
     ;; If some input is pending, problemspace says "sorry", which
     ;; isn't nice; this might happen e.g. when they invoke the
     ;; game by clicking the menu bar.  So discard any pending
     ;; input before zoning out.

     (if (input-pending-p)
      (discard-input))
     (funcall pgm)
     (message "Zoning...sorry"))
    (error
     (while (not (input-pending-p))
      (message (format "We were zoning when we wrote %s..." pgm))
      (sit-for 3)
      (message "...here's hoping we didn't hose your buffer!")
      (sit-for 3)))
    (quit (ding) (message "Zoning...sorry")))
   (when ct (modify-frame-parameters f (list (cons 'cursor-type ct)))))
  (kill-buffer outbuf)
  (problemspace-when-idle problemspace-idle)
  (problemspace-stop)
  (kill-buffer (current-buffer))))


;;;; ProblemSpace when idle, or not.

(defun problemspace-when-idle (secs)
 "start ProblemSpace when Emacs has been idle for SECS seconds."
 (interactive "nHow long before I start zoning (seconds): ")
 (or (<= secs 0)
  (let ((timer (get 'problemspace 'timer)))
   (or (eq timer t)
    (timerp timer)))
  (put 'problemspace 'timer (run-with-idle-timer secs t 'problemspace))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My problemspace stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun problemspace-run ()
 (let ((ops [
	     problemspace-shift-down
	     ]))
  (goto-char (point-min))
  (while (not (input-pending-p))
   (funcall (elt ops (random (length ops))))
   (goto-char (point-min))
   (sit-for 0 0.04))))

(defun problemspace-stop ()
 (interactive)
 (problemspace-leave-me-alone)
 (problemspace-unset-colors))

(defun problemspace-display-matrix ()
 (problemspace-start-matrix
  (lambda () (+ (random 20) 10))))

(defun problemspace-start-matrix (&optional random-style)
 (let* ((specs (apply
		'vector
		(let (res)
		 (mapcar (lambda (ent)
			  (let* ((beg (car ent))
				 (end (cdr ent))
				 (amt (if random-style (funcall random-style)
				       (- (random 7) 3))))
			   (when (< (- end (abs amt)) beg)
			    (setq amt (random (- end beg))))
			   (unless (= 0 amt)
			    (setq res
			     (cons
			      (vector amt beg end)
			      res)))))
		  (problemspace-line-specs))
		 res)))
	(n (length specs))
	(slow-down 10.0)
	amt aamt cut paste txt i ent)
  (progn
   (setq mycnt 0)
   (while (not (input-pending-p))
    (setq i 0)
    (setq mycnt (1+ mycnt))
    (while (< i n)
     (setq ent (aref specs i))
     (setq amt
      (ps-slow-down
       slow-down
       mycnt
       (float (aref ent 0)))
      aamt (abs amt))
     (goto-char (aref ent 1))		; go to beginning
     (setq txt (buffer-substring (point) (+ (point) aamt)))
     (delete-char aamt)
     (goto-char				; go to end
      (- (aref ent 2) (abs amt)))
     (insert txt)
     (setq i (1+ i)))
    (sit-for 0.02)))))

(defun ps-slow-down (slow-down cnt amt)
 "Slow  down  by only  moving  a  fractional  amount based  on  delay.
Basically if we  want to move amt, but over a  certain number of clock
cycles, so for  instance, if we move amount normally,  we want to move
amt /  slowdown over the  same period.  This involves  determining how
far is appropriate  to move.  The formula for  this is basically going
to be a thing where we multiply the two out and subtract"
  (-
  (truncate
   (* cnt
    (/ amt slow-down)))
  (truncate
   (* (- cnt 1)
    (/ amt slow-down)))))

(defun transpose-screen ()
 (interactive)
 (let* ((i 0)
	(specs (problemspace-line-specs))
	(mark (save-excursion
	       (goto-char (window-start))
	       (point)))
	(x (save-excursion
	    (goto-char (window-start))
	    (while (< (point) (window-end))
	     (setq i (+ i 1))
	     (forward-line 1))
	    i))
	(y (save-excursion
	    (goto-char (window-start))
	    (next-line (- x 1))
	    (if (looking-at "\\([^\n]+\\)")
	     (- (match-end 1) (match-beginning 1))
	     0))))
  (progn
   (goto-char (window-start))
   (message (concat "<" (int-to-string x) "><" (int-to-string y) ">"))
   (next-line (- x 1))
   (end-of-line)
   (insert "                                                                                                   ")
   ;; (insert "hello")
   (beginning-of-line)
   (forward-char (- x 1))
   (my-rectangle-transpose mark (point))
   )))

 (defun problemspace-pgm-flip-down-lockstep ()
  (problemspace-pgm-rotate (lambda () -1)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; End my problemspace stuff
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defun problemspace-leave-me-alone ()
  "Don't problemspace out when Emacs is idle."
  (interactive)
  (let ((timer (get 'problemspace 'timer)))
   (and (timerp timer) (cancel-timer timer)))
  (put 'problemspace 'timer t)
  (message "Disabling problemspace"))


 ;;;; problemspace-pgm-jitter

 (defun problemspace-shift-up ()
  (let* ((b (point))
	 (e (progn
	     (end-of-line)
	     (if (looking-at "\n") (1+ (point)) (point))))
	 (s (buffer-substring b e)))
   (delete-region b e)
   (goto-char (point-max))
   (insert s)))

 (defun problemspace-shift-down ()
  (goto-char (point-max))
  (forward-line -1)
  (beginning-of-line)
  (let* ((b (point))
	 (e (progn
	     (end-of-line)
	     (if (looking-at "\n") (1+ (point)) (point))))
	 (s (buffer-substring b e)))
   (delete-region b e)
   (goto-char (point-min))
   (insert s)))

 (defun problemspace-shift-left ()
  (while (not (eobp))
   (or (eolp)
    (let ((c (following-char)))
     (delete-char 1)
     (end-of-line)
     (insert c)))
   (forward-line 1)))

 (defun problemspace-shift-right ()
  (while (not (eobp))
   (end-of-line)
   (or (bolp)
    (let ((c (preceding-char)))
     (delete-backward-char 1)
     (beginning-of-line)
     (insert c)))
   (forward-line 1)))

 (defun problemspace-pgm-jitter ()
  (let ((ops [
	      problemspace-shift-left
	      problemspace-shift-left
	      problemspace-shift-left
	      problemspace-shift-left
	      problemspace-shift-right
	      problemspace-shift-down
	      problemspace-shift-down
	      problemspace-shift-down
	      problemspace-shift-down
	      problemspace-shift-down
	      problemspace-shift-up
	      ]))
   (goto-char (point-min))
   (while (not (input-pending-p))
    (funcall (elt ops (random (length ops))))
    (goto-char (point-min))
    (sit-for 0 10))))


 ;;;; problemspace-pgm-whack-chars

 (defun problemspace-pgm-whack-chars ()
  (let ((tbl (copy-sequence (get 'problemspace-pgm-whack-chars 'wc-tbl))))
   (while (not (input-pending-p))
    (let ((i 48))
     (while (< i 122)
      (aset tbl i (+ 48 (random (- 123 48))))
      (setq i (1+ i)))
     (translate-region (point-min) (point-max) tbl)
     (sit-for 0 2)))))

 (put 'problemspace-pgm-whack-chars 'wc-tbl
  (let ((tbl (make-string 128 ?x))
	(i 0))
   (while (< i 128)
    (aset tbl i i)
    (setq i (1+ i)))
   tbl))

 ;;;; problemspace-pgm-dissolve

 (defun problemspace-remove-text ()
  (let ((working t))
   (while working
    (setq working nil)
    (save-excursion
     (goto-char (point-min))
     (while (not (eobp))
      (if (looking-at "[^(){}\n\t ]")
       (let ((n (random 5)))
	(if (not (= n 0))
	 (progn
	  (setq working t)
	  (forward-char 1))
	 (delete-char 1)
	 (insert " ")))
       (forward-char 1))))
    (sit-for 0 2))))

 (defun problemspace-pgm-dissolve ()
  (problemspace-remove-text)
  (problemspace-pgm-jitter))


 ;;;; problemspace-pgm-explode

 (defun problemspace-exploding-remove ()
  (let ((i 0))
   (while (< i 20)
    (save-excursion
     (goto-char (point-min))
     (while (not (eobp))
      (if (looking-at "[^*\n\t ]")
       (let ((n (random 5)))
	(if (not (= n 0))
	 (forward-char 1))
	(insert " ")))
      (forward-char 1)))
    (setq i (1+ i))
    (sit-for 0 2)))
  (problemspace-pgm-jitter))

 (defun problemspace-pgm-explode ()
  (problemspace-exploding-remove)
  (problemspace-pgm-jitter))


 ;;;; problemspace-pgm-putz-with-case

 ;; Faster than `problemspace-pgm-putz-with-case', but not as good: all
 ;; instances of the same letter have the same case, which produces a
 ;; less interesting effect than you might imagine.
 (defun problemspace-pgm-2nd-putz-with-case ()
  (let ((tbl (make-string 128 ?x))
	(i 0))
   (while (< i 128)
    (aset tbl i i)
    (setq i (1+ i)))
   (while (not (input-pending-p))
    (setq i ?a)
    (while (<= i ?z)
     (aset tbl i
      (if (zerop (random 5))
       (upcase i)
       (downcase i)))
     (setq i (+ i (1+ (random 5)))))
    (setq i ?A)
    (while (<= i ?z)
     (aset tbl i
      (if (zerop (random 5))
       (downcase i)
       (upcase i)))
     (setq i (+ i (1+ (random 5)))))
    (translate-region (point-min) (point-max) tbl)
    (sit-for 0 2))))

 (defun problemspace-pgm-putz-with-case ()
  (goto-char (point-min))
  (while (not (input-pending-p))
   (let ((np (+ 2 (random 5)))
	 (pm (point-max)))
    (while (< np pm)
     (goto-char np)
     (let ((prec (preceding-char))
	   (props (text-properties-at (1- (point)))))
      (insert (if (zerop (random 2))
	       (upcase prec)
	       (downcase prec)))
      (set-text-properties (1- (point)) (point) props))
     (backward-char 2)
     (delete-char 1)
     (setq np (+ np (1+ (random 5))))))
   (goto-char (point-min))
   (sit-for 0 2)))


 ;;;; problemspace-pgm-rotate

 (defun problemspace-line-specs ()
  (let (ret)
   (save-excursion
    (goto-char (window-start))
    (while (< (point) (window-end))
     ; (when (looking-at "[\t ]*\\([^\n]+\\)")
     (when (looking-at "\\(.*\\)")
      (setq ret (cons (cons (match-beginning 1) (match-end 1)) ret)))
     (forward-line 1)))
   ret))

 (defun problemspace-pgm-rotate (&optional random-style)
  (let* ((specs (apply
		 'vector
		 (let (res)
		  (mapcar (lambda (ent)
			   (let* ((beg (car ent))
				  (end (cdr ent))
				  (amt (if random-style
					(funcall random-style)
					(- (random 7) 3))))
			    (when (< (- end (abs amt)) beg)
			     (setq amt (random (- end beg))))
			    (unless (= 0 amt)
			     (setq res
			      (cons
			       (vector amt beg (- end (abs amt)))
			       res)))))
		   (problemspace-line-specs))
		  res)))
	 (n (length specs))
	 amt aamt cut paste txt i ent)
   (while (not (input-pending-p))
    (setq i 0)
    (while (< i n)
     (setq ent (aref specs i))
     (setq amt (aref ent 0) aamt (abs amt))
     (if (> 0 amt)
      (setq cut 1 paste 2)
      (setq cut 2 paste 1))
     (goto-char (aref ent cut))
     (setq txt (buffer-substring (point) (+ (point) aamt)))
     (delete-char aamt)
     (goto-char (aref ent paste))
     (insert txt)
     (setq i (1+ i)))
    (sit-for 1)
    )))

 (defun problemspace-line-specs-vertical ()
  (let (ret)
   (save-excursion
    (goto-char (window-start))
    (while (< (point) (window-end))
     (when (looking-at "[\t ]*\\([^\n]+\\)")
      (setq ret (cons (cons (match-beginning 1) (match-end 1)) ret)))
     (forward-line 1)))
   ret))

 (require 'cl)

 (defun my-rectangle-transpose (beg end)
  "Transposes the rectangle delimited by point and mark. Before calling
this, make space around the rectangle if its height and width are not
the same."
  (interactive "*r")
  (let* ((old (delete-extract-rectangle beg end))
	 (x (length (first old)))
	 (y (length old))
	 (new (make-vector x nil)))
   (loop for line in old and j below y do
    (loop for i below x do
     (if (zerop j) (aset new i (make-string y ?\ )))
     (aset (aref new i) j (aref line i))))
   (goto-char (min beg end))
   (insert-rectangle (mapcar 'identity new))))

 (defun buffer-substring-vertical ()
  "take a substring, vertically")

 (defun insert-vertical ()
  "insert a string, vertically, at point")

 (defun problemspace-pgm-rotate-LR-lockstep ()
  (problemspace-pgm-rotate (lambda () 1)))

 (defun problemspace-pgm-rotate-RL-lockstep ()
  (problemspace-pgm-rotate (lambda () -1)))

 (defun problemspace-pgm-rotate-LR-variable ()
  (problemspace-pgm-rotate (lambda () (1+ (random 3)))))

 (defun problemspace-pgm-rotate-RL-variable ()
  (problemspace-pgm-rotate (lambda () (1- (- (random 3))))))


 ;;;; problemspace-pgm-drip

 (defun problemspace-cpos (pos)
  (buffer-substring pos (1+ pos)))

 (defun problemspace-fret (pos)
  (let* ((case-fold-search nil)
	 (c-string (problemspace-cpos pos))
	 (hmm (cond
	       ((string-match "[a-z]" c-string) (upcase c-string))
	       ((string-match "[A-Z]" c-string) (downcase c-string))
	       (t " "))))
   (do ((i 0 (1+ i))
	(wait 0.5 (* wait 0.8)))
    ((= i 20))
    (goto-char pos)
    (delete-char 1)
    (insert (if (= 0 (% i 2)) hmm c-string))
    (sit-for wait))
   (delete-char -1) (insert c-string)))

 (defun problemspace-fall-through-ws (c col wend)
  (let ((fall-p nil)                    ; todo: move outward
	(wait 0.15)
	(o (point))                     ; for terminals w/o cursor hiding
	(p (point)))
   (while (progn
	   (forward-line 1)
	   (move-to-column col)
	   (looking-at " "))
    (setq fall-p t)
    (delete-char 1)
    (insert (if (< (point) wend) c " "))
    (save-excursion
     (goto-char p)
     (delete-char 1)
     (insert " ")
     (goto-char o)
     (sit-for (setq wait (* wait 0.8))))
    (setq p (1- (point))))
   fall-p))

 (defun problemspace-pgm-drip (&optional fret-p pancake-p)
  (let* ((ww (1- (window-width)))
	 (wh (window-height))
	 (mc 0)                         ; miss count
	 (total (* ww wh))
	 (fall-p nil))
   (goto-char (point-min))
   ;; fill out rectangular ws block
   (while (not (eobp))
    (end-of-line)
    (let ((cc (current-column)))
     (if (< cc ww)
      (insert (make-string (- ww cc) ? ))
      (delete-char (- ww cc))))
    (unless (eobp)
     (forward-char 1)))
   ;; what the hell is going on here?
   (let ((nl (- wh (count-lines (point-min) (point)))))
    (when (> nl 0)
     (let ((line (concat (make-string (1- ww) ? ) "\n")))
      (do ((i 0 (1+ i)))
       ((= i nl))
       (insert line)))))
   ;;
   (catch 'done; ugh
    (while (not (input-pending-p))
     (goto-char (point-min))
     (sit-for 0)
     (let ((wbeg (window-start))
	   (wend (window-end)))
      (setq mc 0)
      ;; select non-ws character, but don't miss too much
      (goto-char (+ wbeg (random (- wend wbeg))))
      (while (looking-at "[ \n\f]")
       (if (= total (setq mc (1+ mc)))
	(throw 'done 'sel)
	(goto-char (+ wbeg (random (- wend wbeg))))))

      ;; character animation sequence
      (let ((p (point)))
       (when fret-p (problemspace-fret p))
       (goto-char p)
       (setq fall-p (problemspace-fall-through-ws
		     (problemspace-cpos p) (current-column) wend))))
     ;; assuming current-column has not changed...
     (when (and pancake-p
	    fall-p
	    (< (count-lines (point-min) (point))
	     wh))
      (previous-line 1)
      (forward-char 1)
      (sit-for 0.137)
      (delete-char -1)
      (insert "@")
      (sit-for 0.137)
      (delete-char -1)
      (insert "*")
      (sit-for 0.137)
      (delete-char -1)
      (insert "_"))))))

 (defun problemspace-pgm-drip-fretfully ()
  (problemspace-pgm-drip t))

 (defun problemspace-pgm-five-oclock-swan-dive ()
  (problemspace-pgm-drip nil t))

 (defun problemspace-pgm-martini-swan-dive ()
  (problemspace-pgm-drip t t))


 ;;;; problemspace-pgm-paragraph-spaz

 (defun problemspace-pgm-paragraph-spaz ()
  (if (memq (problemspace-orig major-mode) '(text-mode fundamental-mode))
   (let ((fill-column fill-column)
	 (fc-min fill-column)
	 (fc-max fill-column)
	 (max-fc (1- (frame-width))))
    (while (sit-for 0.1)
     (fill-paragraph 1)
     (setq fill-column (+ fill-column (- (random 5) 2)))
     (when (< fill-column fc-min)
      (setq fc-min fill-column))
     (when (> fill-column max-fc)
      (setq fill-column max-fc))
     (when (> fill-column fc-max)
      (setq fc-max fill-column))))
   (message "Zoning... (problemspace-pgm-rotate)")
   (problemspace-pgm-rotate)))


 ;;;; problemspace-pgm-stress

 (defun problemspace-pgm-stress ()
  (goto-char (point-min))
  (let (lines bg mode-line-fg mode-line-bg mode-line-box)
   (while (< (point) (point-max))
    (let ((p (point)))
     (forward-line 1)
     (setq lines (cons (buffer-substring p (point)) lines))))
   (sit-for 5)
   (unwind-protect
    (progn
     (when (display-color-p)
      (setq bg (face-background 'default)
       mode-line-box (face-attribute 'mode-line :box)
       mode-line-fg (face-attribute 'mode-line :foreground)
       mode-line-bg (face-attribute 'mode-line :background))
      (set-face-attribute 'mode-line nil
       :foreground bg
       :background bg
       :box nil))

     (let ((msg "Zoning... (problemspace-pgm-stress)"))
      (while (not (string= msg ""))
       (message (setq msg (substring msg 1)))
       (sit-for 0.05)))

     (while (not (input-pending-p))
      (when (< 50 (random 100))
       (goto-char (point-max))
       (forward-line -1)
       (unless (eobp)
	(let ((kill-whole-line t))
	 (kill-line)))
       (goto-char (point-min))
       (when lines
	(insert (nth (random (1- (length lines))) lines))))
      (message (concat (make-string (random (- (frame-width) 5)) ? )
		"grrr"))
      (sit-for 0.1)))
    (when mode-line-fg
     (set-face-attribute 'mode-line nil
      :foreground mode-line-fg
      :background mode-line-bg
      :box mode-line-box)))))

 (defun problemspace-set-colors ()
  ""
  (interactive)
  (set-background-color "black")
  (set-foreground-color "LimeGreen")
  (global-font-lock-mode 0))

 (defun problemspace-unset-colors ()
  ""
  (interactive)
  (set-background-color "brightwhite")
  ;   (set-background-color "white")
  (set-foreground-color "black")
  (global-font-lock-mode 1))

 (provide 'problemspace)
 ;;; problemspace.el ends here
