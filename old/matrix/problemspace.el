
;;; problemspace.el --- Matrix-like FRDCSA/UniLang world-state symbolic-VR HCI

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
;; the Matrix

;;; Code:

;; start writing Sun Feb 13 02:15:02 EST 2005

(require 'timer)
(require 'tabify)
(eval-when-compile (require 'cl))

(defvar ps-max-rate 30.0
 "ProblemSpace message speed limit")

(defvar ps-min-rate 10.0
 "ProblemSpace message speed minimum")

(defvar ps-denominator 10.0
 "Denominator in  distance moved  per one clock  cycle for rate  of 1,
must be float")

;; (setq ps-loop-delay 0.02)

(defvar ps-loop-delay 0.3
 "How long delay at end of loop is.")

(defvar ps-cleanup-mode t
 "ProblemSpace message speed limit")

(defvar ps-running-p nil
 "Set to t when problemspace is running")

(defvar ps-message-queue nil
 "Stores incoming messages")

(defvar ps-specs nil
 "Stores specs")

(setq ps-max-rate 15)
(setq ps-min-rate 3)
(setq ps-loop-delay 0.001)
(setq ps-denominator 50.0)
(setq ps-cleanup-mode nil)
(setq ps-running-p nil)

(defvar ps-idle 20
 "*Seconds to idle before starting ProblemSpace.")

;; Vector of functions that problemspace out.  `problemspace' will execute one of
;; these functions, randomly chosen.  The chosen function is invoked
;; in the *problemspace* buffer, which contains the text of the selected
;; window.  If the function loops, it *must* periodically check and
;; halt if `input-pending-p' is t (because quitting is disabled when
;; Emacs idle timers are run).

(defvar ps-programs [
		     ps-display-matrix
		     ])

(defmacro ps-orig (&rest body)
 `(with-current-buffer (get 'problemspace 'orig-buffer)
   ,@body))

;;;###autoload
(defun problemspace ()
 "ProblemSpace out, completely."
 (setq ps-running-p t)
 (interactive)
 (ps-set-colors)
 (find-file "/var/lib/myfrdcsa/codebases/internal/problemspace/data/matrixdata")
 ; (ps-load-music)
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
  (let ((pgm (elt ps-programs (random (length ps-programs))))
	(ct (and f (frame-parameter f 'cursor-type))))
   (when ct (modify-frame-parameters f '((cursor-type . (bar . 0)))))
   (condition-case nil
    (progn
     (message "Starting ProblemSpace... (%s)" pgm)
     (garbage-collect)
     ;; If some input is pending, problemspace says "sorry", which
     ;; isn't nice; this might happen e.g. when they invoke the
     ;; game by clicking the menu bar.  So discard any pending
     ;; input before starting ProblemSpace

     (if (input-pending-p)
      (discard-input))
     (funcall pgm)
     (message "Starting ProblemSpace despite pending input"))
    (error
     (while (not (input-pending-p))
      (message (format "We were starting ProblemSpace when we wrote %s..." pgm))
      (sit-for 3)
      (message "Perhaps the buffer is still intact.")
      (sit-for 3)))
    (quit (ding) (message "Starting ProblemSpace")))
   (when ct (modify-frame-parameters f (list (cons 'cursor-type ct)))))
  (kill-buffer outbuf)
  (ps-when-idle ps-idle)
  (ps-stop)
  (kill-buffer (current-buffer))))

(defun ps-load-music ()
 "Gimmick to load music when starting the Matrix"
 (shell-command "mplayer ~/matrix.mp3 &"))

;;;; ProblemSpace when idle, or not.

(defun ps-when-idle (secs)
 "start ProblemSpace when Emacs has been idle for SECS seconds."
 (interactive "nHow long before I start ProblemSpace (seconds): ")
 (or (<= secs 0)
  (let ((timer (get 'problemspace 'timer)))
   (or (eq timer t)
    (timerp timer)))
  (put 'problemspace 'timer (run-with-idle-timer secs t 'problemspace))))

(defun ps-stop ()
 (interactive)
 (setq ps-running-p nil)
 (ps-leave-me-alone)
 (ps-unset-colors))

(defun ps-display-matrix ()
 (ps-start-matrix
  (lambda () (+ (random (- ps-max-rate ps-min-rate))
		 ps-min-rate))))

(defun ps-start-matrix (&optional random-style)
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
			      (vector amt beg end
			       (random (truncate ps-denominator)))
			      res)))))
		  (ps-line-specs))
		 res)))
	(n (length specs))
	amt aamt cut paste txt i ent)
  (progn
   (setq ps-specs specs)
   (setq mycnt 0)
   (while (not (input-pending-p))
    (setq i 0)
    (setq mycnt (1+ mycnt))
    (while (< i n)
     (setq ent (aref specs i))
     (setq amt
      (ps-slow-down
       mycnt
       (float (aref ent 0))
       (aref ent 3))
      aamt (abs amt))
     (goto-char (aref ent 1))		; go to beginning
     (setq txt (buffer-substring (point) (+ (point) aamt)))
     (delete-char aamt)
     (goto-char				; go to end
      (- (aref ent 2) (abs amt)))
     (if (ps-queued i)
      (insert (ps-get-from-queue i aamt))
      (if ps-cleanup-mode
       (insert (make-string aamt ?\ ))
       (insert txt)))
     (setq i (1+ i)))
    (sit-for ps-loop-delay))
   )))

(defun ps-queued (pos)
 "Determine whether a queued object exists in the given position"
 (and
  (stringp (aref ps-message-queue pos))
  (> (length (aref ps-message-queue pos)) 0)))

(defun ps-get-from-queue (pos len)
 "Extract len from queue position pos"
 (let ((item (aref ps-message-queue pos)))
  (if (> (length item) len)
   (let*
    ((res (substring item 0 len)))
    (aset ps-message-queue pos (substring item len (length item)))
    res)
   ; do insert no problem
   (progn
    (aset ps-message-queue pos "")
    (concat item (make-string (- len (length item)) ?\ ))))))

(defun ps-slow-down (cnt amt seed)
 "Slow  down  by only  moving  a  fractional  amount based  on  delay.
Basically if we  want to move amt, but over a  certain number of clock
cycles, so for  instance, if we move amount normally,  we want to move
amt /  slowdown over the  same period.  This involves  determining how
far is appropriate  to move.  The formula for  this is basically going
to be a thing where we multiply the two out and subtract"
 (-
  (truncate
   (* (+ cnt seed)
    (/ amt ps-denominator)))
  (truncate
   (* (- (+ cnt seed) 1)
    (/ amt ps-denominator)))))

(defun ps-leave-me-alone ()
 "Don't problemspace out when Emacs is idle."
 (interactive)
 (let ((timer (get 'problemspace 'timer)))
  (and (timerp timer) (cancel-timer timer)))
 (put 'problemspace 'timer t)
 (message "Disabling problemspace"))

(defun ps-line-specs ()
 (setq ps-message-queue nil)
 (setq ps-specs nil)
 (let (ret)
  (save-excursion
   (goto-char (window-start))
   (while (< (point) (window-end))
    ; (when (looking-at "[\t ]*\\([^\n]+\\)")
    (when (looking-at "\\(.*\\)")
     (setq ps-message-queue (cons "" ps-message-queue))
     (setq ret (cons (cons (match-beginning 1) (match-end 1)) ret)))
    (forward-line 1)))
  (setq ps-message-queue (apply 'vector ps-message-queue))
  ret))

(defun ps-set-colors ()
 ""
 (interactive)
 (set-background-color "black")
 (set-foreground-color "LimeGreen")
 (global-font-lock-mode 0))

(defun ps-unset-colors ()
 ""
 (interactive)
 (set-background-color "brightwhite")
 ;; (set-background-color "white")
 (set-foreground-color "black")
 (global-font-lock-mode 1))

(defun ps-queue-message-old (contents)
 "Receive and queue a message for display"
 ;; insert it into a random line
 (if (ps-verify-ps-is-running)
  (let* ((n (length ps-specs))
	 (ent (aref ps-specs (random n))))
   (goto-char (aref ent 2))
   (backward-char (length contents))
   (delete-char (length contents))
   (insert contents))))

(defun ps-queue-message (contents)
 "Receive and queue a message for display"
 ;; insert it into a random line
 (if (ps-verify-ps-is-running)
   (aset ps-message-queue (random (length ps-message-queue)) contents)))

(defun ps-verify-ps-is-running ()
 "Determine whether ProblemSpace is  running.  If it isn't, attempt to
start it.  Ultimately, return t if it is running, nil otherwise."
 (if ps-running-p
  t
  (if (problemspace)
   t
   nil)))

(defun ps-change-speed-of-line (line speed)
 "Alter the speed of a line")

(provide 'problemspace)
 ;;; problemspace.el ends here
