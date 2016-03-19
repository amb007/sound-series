;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2009 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :ss)

(defparameter *gym1-1*
  (let ((p1 '((1 nil) (1 9) (1 12)
	      (1 10) (1 9) (1 4)
	      (1 2) (1 4) (1 5)
	      (3 0))))
    `(,@(rep 4 '((3 nil)))
	,@p1
	,@(rep 4 '((3 -3)))
	,@p1
	(3 4)
	(3 9)
	(3 -5)
	(3 -5)
	; 21
	(3 -5)
	(1 0) (1 2) (1 3)
	(1 7) (1 5) (1 2)
	(1 5) (1 3) (1 2)
	(3 5)
	(2 5) (1 5)
	(1 7) (1 8) (1 10)
	(1 12) (1 3) (1 5)
	(1 7) (1 5) (1 2)
	(3 5)
	(2 5) (1 5)
	; 1.
	,@(rep
	   2
	   `((3 10)
	     (3 9)
	     (1 2) (1 0) (1 2)
	     ,@(rep 2 '((1 4) (1 5) (1 7)))
	     (2 -3) (1 -2)
	     (3 3)
	     (3 5)))
	; 2.
	(3 10)
	(3 8)
	(1 2) (1 3) (1 8)
	,@(rep 2 '((1 7) (1 5) (1 3)))
	(2 -4) (1 -2)
	(3 3) (3 5))))

(defparameter *gym1-2*
  `(,@(rep 8 '((1 nil) (2 -10 -7 -3)
	       (1 nil) (2 -12 -8 -3)))
      (1 nil) (2 -12 -8 -3)
      (1 nil) (2 -10 -7 -3)
      (1 nil) (2 -14 -10)
      (1 nil) (2 -10 -7 -2)
      ; 21
      (1 nil) (2 -4 0 5)
      (1 nil) (2 0 3 7)
      (1 nil) (2 -2 2 7)
      (1 nil) (2 -2 2 7)
      (1 nil) (2 -5 0 5)
      (1 nil) (2 -4 0 5)
      (1 nil) (2 0 3 8)
      (1 nil) (2 0 3 7)
      (1 nil) (2 -2 2 7)
      (1 nil) (2 -5 0 5)
      (1 nil) (2 -4 0 5)
      ; 1.
      ,@(rep
	 2
	 '((1 nil) (2 -10 -5 -2)
	   (1 nil) (2 -12 -8 -3)
	   (1 nil) (2 -10 -7 -3)
	   (1 nil) (2 -8 -5 0)
	   (1 nil) (2 -12 -8 -3 0)
	   (1 nil) (1 -12 -7) (1 -10 -7)
	   (3 -9 -4 0)
	   (3 -7 -3 0)))
      ; 2.
      (1 nil) (2 -10 -5 -2)
      (1 nil) (2 -12 -7 -4 0)
      (1 nil) (2 -12 -9 -4)
      (1 nil) (2 -9 -5 0)
      (1 nil) (2 -12 -9 -4 0)
      (1 nil) (1 -12 -7) (1 -10 -7)
      (3 -9 -4 0)
      (3 -7 -3 0)))

(defparameter *gym1-3*
  `(,@(rep 8 '((3 -14)
	       (3 -19)))
      (3 -15)
      (3 -22)
      (3 -20)
      (3 -20)
      ; 21
      (3 -19)
      (3 -24)
      ,@(rep 5 '((3 -19)))
      ,@(rep 4 '((3 -19)))
      ; 1.
      ,@(rep
	 2
	 `((3 -17)
	   (3 -15)
	   (3 -22)
	   ,@(rep 3 '((3 -17)))
	   (3 -12 -2)
	   (3 -19 -12 -7)))
      ; 2.
      ,@(rep 6 '((3 -17)))
      (3 -12 -2)
      (3 -19 -12 -7)))

(defparameter *gym1-3b*
  (let ((p1 '((1 nil) (2 -7) (1 nil) (2 -9) (1 nil) (2 -9)))
	(p2 '((3 nil))))
    `(,@(rep 23 p2)
	,@p1
	,@(rep 2 p2)
	,@p1
	; 1.
	,@(rep
	   2
	   `(,@(rep 5 p2)
	       (1 nil) (1 -10) (1 -5)
	       ,@(rep 2 p2)))
	; 2.
	,@(rep 5 p2)
	(1 nil) (1 -10) (1 -5)
	,@(rep 2 p2))))


(defun double-d (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (timebase ts)
   :duration (* 2 (duration ts))
   :key (key ts)
   :ampl (ampl ts)
   :instrument (instrument ts)))

(defun gen-gym1 (maxtb)
  (declare (optimizable-series-function))
  (let ((v (lambda (ts sfn dfn)
	     (violin ts sfn (lambda (d) (* 4 (funcall dfn d)))))))
    (let ((c1 (scan-notes *gym1-1* v))
	  (c2 (scan-notes *gym1-2* #'piano))
	  (c3 (scan-notes *gym1-3* #'piano))
	  (c3b (scan-notes *gym1-3b* #'piano)))
      (gen-sound maxtb 1 c1 c2 c3 c3b))))