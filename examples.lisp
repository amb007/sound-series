;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2009 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :ss)

;;;; Popeye the sailor man

(defparameter *popi-1*
  '((1 -15) (1 -12) (1 -12) (1 -12) (2 -14) (1 -15) (5 -12)
    (1 -12) (1 -10) (1 -14) (1 -10) (2 -7) (1 -10) (5 -12)
    (1 -12) (1 -10) (1 -14) (1 -10) (1 -7) (1 -8) (1 -10) (1 -12)
      (1 -10) (1 -12) (2 -15)
    (12 nil)))

(defparameter *popi-2*
  '((36 nil)
    (1 -15) (1 -12) (1 -12) (1 -12) (2 -10) (1 -8) (5 -7)))

(defun skew-1 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (+ (timebase ts) 12)
   :duration (* 1.5 (duration ts))
   :key (+ 24 (key ts))
   :ampl (* 0.5 (ampl ts))
   :instrument (instrument ts)))

(defun skew-2 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (+ (timebase ts) 4)
   :duration (* 0.25 (duration ts))
   :key (- (key ts) 24)
   :ampl (* 2 (ampl ts))
   :instrument (instrument ts)))

(defun skew-3 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (timebase ts)
   :duration (duration ts)
   :key (+ 12 (key ts))
   :ampl (* 2 (ampl ts))
   :instrument (instrument ts)))

(defun gen-popi-sound (maxtb)
  (declare (optimizable-series-function))
  (let ((c1 (scan-notes *popi-1*))
	(m1 (scan-notes *popi-1* #'drum))
	(e1 (map-fn 'time-sound #'skew-3 (scan-notes *popi-2*)))
	(d1 (map-fn
	     'time-sound
	     #'skew-3
	     (scan-notes
	      '((1 0) (1 0) (2 nil) (1 0) (7 nil))
	      #'maraca))))
    (let ((c2 (map-fn 'time-sound #'skew-1 c1))
	  (c3 (map-fn 'time-sound #'skew-2 c1)))
      (gen-sound maxtb 1 c1 c2 c3 m1 d1 e1))))

;;;; Café de flore

(defparameter *cafe-1*
  '((6 0) (1 -1) (1 -3)
    (3 0) (1 -3) (4 0)
    (2 2) (1 0) (1 -1) (1 -1) (1 -3) (1 -5) (1 -7)
    (8 -8)))

(defparameter *cafe-2*
  '((1 0) (0.5 3) (0.5 0)))

(defparameter *cafe-3*
  '((1 4) (1 6) (1 7) (1 nil)
    (1 nil) (1 7) (1 6) (1 4)))

(defparameter *cafe-3s*
  '((1 2) (1 4) (1 6) (1 nil)
    (1 nil) (1 6) (1 4) (1 2)))

(defparameter *cafe-3a*
  `(,@(rep 2 *cafe-3*) ,@(rep 4 *cafe-3*) ,@(rep 2 *cafe-3*)))

(defparameter *cafe-4*
  '((1 16) (7 nil)
    (1 18) (7 nil)
    (1 19) (7 nil)
    (8 nil)))

(defparameter *cafe-5*
  '((6 11) (1 12) (1 14)
    (3 11) (1 4) (4 11)
    (5 12) (1 14) (1.5 19) (0.5 18)
    (8 16)))

(defparameter *cafe-6*
  '((1 -1) (1 -1) (1 -3) (1 -3) (1 -5) (1 -5) (1 -6) (1 -8)))

(defparameter *cafe-6s*
  '((1 0) (1 0) (1 -1) (1 -1) (1 -3) (1 -3) (1 -5) (1 -6)))

(defparameter *cafe-6e*
  '((1 -1) (1 -1) (1 -3) (1 -3) (1 -5) (1 -5) (1 -6) (5 -6)
    (2 -5) (2 -8)))

(defparameter *cafe-6a*
  `(,@(rep 2 *cafe-6*) ,@(rep 4 *cafe-6s*) ,@*cafe-6* ,@*cafe-6e*))

(defparameter *cafe-7*
  '((6 -1) (1 -3) (1 -5)
    (3 0) (1 -8) (4 0)
    (5 2) (1 4) (1.5 7) (0.5 6)
    (8 4)))

(defparameter *cafe-8*
  '((6 -1) (1 -3) (1 -5)
    (2 -1) (6 0)
    (6 2) (2 4)
    (8 -1)))

(defun scafe-2 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (/ (timebase ts) 3)
   :duration (duration ts)
   :key (key ts)
   :ampl (ampl ts)
   :instrument (instrument ts)))

(defun scafe-3 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (/ (timebase ts) 3)
   :duration (duration ts)
   :key (key ts)
   :ampl (ampl ts)
   :instrument (instrument ts)))

(defun scafe-3b (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (/ (+ 32 (timebase ts)) 3)
   :duration (duration ts)
   :key (key ts)
   :ampl (ampl ts)
   :instrument (instrument ts)))

(defun scafe-4 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (+ 16 (timebase ts))
   :duration (duration ts)
   :key (+ (if (< (mod (timebase ts) 32) 16) -6 +12) 4 (key ts))
   :ampl (* 2 (ampl ts))
   :instrument (instrument ts)))

(defun scafe-4b (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (* 2 (+ 16 (timebase ts)))
   :duration (* 2 (duration ts))
   :key (+ 12 (interval 4) (key ts))
   :ampl (* 2 (ampl ts))
   :instrument (instrument ts)))

(defun scafe-4c (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (* 2 (+ 16 (timebase ts)))
   :duration (* 2 (duration ts))
   :key (+ 0 (key ts))
   :ampl (* 2 (ampl ts))
   :instrument (instrument ts)))

(defun scafe-5 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (* 2 (timebase ts))
   :duration (duration ts)
   :key (+ 12 (key ts))
   :ampl (* 0.5 (ampl ts))
   :instrument (instrument ts)))

(defun scafe-6 (ts)
  (make-instance
   'time-sound
   :sync-clk (sync-clk ts)
   :timebase (/ (timebase ts) 3)
   :duration (duration ts)
   :key (key ts)
   :ampl (ampl ts)
   :instrument (instrument ts)))

(defun gen-cafe-sound (maxtb)
  (declare (optimizable-series-function))
  (let ((c1 (scan-notes *cafe-1*))
	(c2 (map-fn t #'scafe-2 (scan-notes *cafe-2* #'drum)))
	(c1b (map-fn t #'scafe-6 (scan-notes *cafe-1*)))
	(c3 (map-fn t #'scafe-3 (scan-notes (addk -12 *cafe-3a*))))
	(c3b (map-fn t #'scafe-3b (scan-notes (chord (addk -24 *cafe-3a*)))))
	(c4 (scan-notes *cafe-4*))
	(c5 (scan-notes *cafe-5* #'piano))
	(c6 (map-fn t #'scafe-6 (scan-notes (chord *cafe-6a*))))
	(c7 (scan-notes *cafe-7*))
	(c8 (scan-notes *cafe-8*)))
    (gen-sound maxtb 0.8 c2 c3 c3b c6)))

;;;; some rhytm

(defparameter *rhytm-1*
  `(,@(rep 3 '((1 0) (1 nil))) (0.25 0) (0.25 0) (1.5 nil)))

(defparameter *rhytm-2*
  '((1 nil) (1 0)))

(defparameter *rhytm-3*
  (addk -36 *cafe-3*))

(defun gen-rhytm-sound (maxtb)
  (declare (optimizable-series-function))
  (let ((c1 (scan-notes *rhytm-1* #'drum))
	(c2 (scan-notes *rhytm-2* #'maraca))
	(c3 (scan-notes *rhytm-3*)))
    (gen-sound maxtb 1 c1 c2 c3)))