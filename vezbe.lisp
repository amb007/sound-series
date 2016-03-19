;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2011 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :ss)

(defparameter *lovac-joca*
  `(,@(rep 2 '((0.5 0) (0.5 nil) (0.5 2) (0.5 nil)
	       (0.5 0) (1.5 nil)
	       (0.5 0) (0.5 -3) (0.5 -2) (0.5 -5)
	       (1 -7) (1 nil)))
      ,@(rep 2 '((1 -7) (1 nil)
		 (1 -5) (1 nil)
		 (1 -5) (1 nil)
		 (1 -5) (1 -5)
		 (1 2) (1 -5)
		 (1 0) (1 nil)))
      ,@(rep 2 '((0.5 0) (0.5 nil) (0.5 2) (0.5 nil)
	       (0.5 4) (1.5 nil)
		 (0.5 5) (0.5 0) (0.5 -2) (0.5 -5)
		 (1 -7) (1 nil)))
      (1 nil) (0.5 -5) (0.5 -5)
      (0.5 -5) (0.5 -5) (0.5 -5) (0.5 -5)
      (1 0) (1 0)
      (0.5 0) (0.5 -2) (0.5 -3) (0.5 -7)
      (1 nil) (0.5 -5) (0.5 -5)
      (0.5 -5) (0.5 -5) (0.5 -5) (0.5 -5)
      (0.5 0) (0.5 0) (0.5 0) (0.5 0)
      (1 -7) (1 -7)))

(defun frob (l)
  (mapcar
   (lambda (e)
     (destructuring-bind (dur tone) e
       `(,dur ,@(if tone `(,tone ,(+ 12 tone)) `(nil)))))
   l))

(defun frob2 (d l)
  `((,d nil) ,@l))

;;; (gen-sound 72 1 (scan-notes (frob *lovac-joca*) #'piano) (scan-notes (frob2 0.25 *lovac-joca*) #'violin) (scan-notes '((0.5 0) (0.25 nil) (0.25 12)) #'gong))
;;; (gen-sound 72 1  (scan-notes '((0.25 0) (0.25 6) (0.25 nil) (0.125 12) (0.125 0)) #'gong))

(defparameter *sat-d*
  '((1 0) (1 2)
    (1 0) (1 2)
    (1 0) (1 2)
    (2 2)
    (1 nil) (1 2)
    (1 nil) (1 2)
    (1 nil) (1 2)
    (2 2)))

(defparameter *sat-l*
  '((2 nil)
    (2 nil)
    (2 nil)
    (2 nil)
    (1 0) (1 nil)
    (1 0) (1 nil)
    (1 0) (1 nil)
    (2 0)))

;;; (gen-sound 16 1 (scan-notes *sat-l* #'piano) (scan-notes *sat-d* #'violin))

(defparameter *igra-patuljaka-d*
  '((2 0) (1 2)
    (3 nil)
    (3 0)
    (3 nil)
    (2 0) (1 2)
    (3 nil)
    (3 0)
    (3 nil)
    (2 0) (1 2)
    (3 nil)
    (3 0)
    (3 nil)
    (2 0) (1 2)
    (3 nil)
    (3 0)
    (3 nil)))

(defparameter *igra-patuljaka-l*
  '((3 nil)
    (2 0) (1 -1)
    (3 nil)
    (3 0)
    (3 nil)
    (2 0) (1 -1)
    (3 nil)
    (3 0)
    (3 nil)
    (2 0) (1 -1)
    (3 nil)
    (3 0)
    (3 nil)
    (2 0) (1 -1)
    (3 nil)
    (3 0)))

;;; (gen-sound 48 1 (scan-notes *igra-patuljaka-l* #'piano) (scan-notes *igra-patuljaka-d* #'violin))

(defparameter *cadence-mol-a-d*
  '((1 0 4 9) (1 2 5 9) (1 2 4 8)
    (3 0 4 9)))

(defparameter *cadence-mol-a-l*
  '((1 -15) (1 -10) (1 -8)
    (3 -15)))

;;; (gen-sound 6 0.1 (scan-notes *cadence-mol-a-l* #'violin) (scan-notes *cadence-mol-a-d* #'piano))

(defparameter *kornjaca-d*
  '((1 0) (1 4) (1 5) (1 7)
    (1 9) (1 7) (1 5) (1 4)
    (1 2) (1 5) (1 7) (1 9)
    (1 11) (1 9) (1 7) (1 5)
    (1 4) (1 7) (1 9) (1 11)
    (1 12) (1 11) (1 9) (1 7)
    (1 5) (1 9) (1 11) (1 12)
    (1 14) (1 12) (1 11) (1 9)
    (1 7) (1 11) (1 12) (1 14)
    (1 16) (1 14) (1 12) (1 11)))

(defparameter *kornjaca-l*
  '((1 -12) (1 -8) (1 -7) (1 -5)
    (1 -3) (1 -5) (1 -7) (1 -8)
    (1 -10) (1 -7) (1 -5) (1 -3)
    (1 -1) (1 -3) (1 -5) (1 -7)
    (1 -8) (1 -5) (1 -3) (1 -1)
    (1 0) (1 -1) (1 -3) (1 -5)
    (1 -7) (1 -3) (1 -1) (1 0)
    (1 2) (1 0) (1 -1) (1 -3)
    (1 -5) (1 -1) (1 0) (1 2)
    (1 4) (1 2) (1 0) (1 -1)))

;;; (gen-sound 40 1 (scan-notes *kornjaca-l* #'piano) (scan-notes *kornjaca-d* #'violin))

(defparameter *cadence-dur-c-d*
  '((1 4 7 12) (1 5 9 12) (1 2 7 11)
    (3 4 7 12)))

(defparameter *cadence-dur-c-l*
  '((1 -12) (1 -7) (1 -5)
    (3 -12)))

;;; (gen-sound 12 1 (scan-notes *cadence-dur-c-d* #'violin) (scan-notes *cadence-dur-c-l* #'piano))

(defparameter *cadence*
  '((1 0) (1 4) (1 7)
    (1 4) (2 0)
    (1 0) (1 5) (1 9)
    (1 5) (2 0)
    (1 -1) (1 2) (1 7)
    (1 2) (2 11)
    (1 12) (1 7) (1 4)
    (1 7) (2 0)))

;;; (gen-sound 24 1 (scan-notes *cadence* #'piano))

(defparameter *nobody-knows-d*
  (muld
   1/2
   (addk
    12
    (with-mnemonics ()
      '((1 mi4) (3 nil)
	(3 nil) (1 re4)
	(1 mi4) (1 mi4) (2 mi4)
	(4 mi4)
	(1 mi4) (3 nil)
	(4 nil)
	(4 nil)
	(2 fa4) (2 sol4)
	(1 mi4) (3 nil)
	(3 nil) (1 re4)
	(1 mi4) (1 mi4) (2 mi4)
	(4 mi4)
	(3 sol4) (1 mi4)
	(2 re4) (2 mi4)
	(4 do4)
	(4 nil))))))

(defparameter *nobody-knows-l*
  (muld
   1/2
   (addk
    12
    (with-mnemonics ()
      '((1 nil) (2 sol3) (1 la3)
	(3 do4) (1 nil)
	(4 nil)
	(4 nil)
	(1 nil) (2 sol3) (1 la3)
	(3 do4) (1 do4)
	(2 la3) (2 sol3)
	(4 sol3)
	(1 nil) (2 sol3) (1 la3)
	(3 do4) (1 nil)
	(4 nil)
	(4 nil)
	(4 nil)
	(4 nil)
	(4 nil)
	(4 do4))))))

(defparameter *nobody-knows-p*
  (muld
   1/2
   (with-mnemonics ()
     '((4 do2 sol2 mi3)
       (4 do2 la2 mi3)
       (2 do3 sol3) (2 (1+ sol3))
       (2 do3 la3) (2 (1- la3))
       (4 do2 sol2 mi3)
       (4 fa2 do3 re3)
       (4 sol2 fa3)
       (4 sol1 fa3)
       (4 do2 sol2 mi3)
       (4 do2 la2 fa3)
       (2 do3 sol3) (2 (1+ sol3))
       (2 do3 la3) (2 (1- si3))
       (4 la2 la3 (1+ do4))
       (2 re3 (1+ fa3) do4) (2 sol2 fa3 si3)
       (2 do2 la2 fa3) (2 fa2 re3)
       (4 do2 sol2 mi3)))))

;;; (gen-sound 32 1 (scan-notes *nobody-knows-d* #'piano) (scan-notes *nobody-knows-l* #'piano) (scan-notes *nobody-knows-p* #'piano))

(defparameter *deda-meda-d*
  `(,@(rep 2 '((1 -3) (1 -3) (1 -3) (1 -3)
	       (2 -5) (1 -7) (1 -5)
	       (1 -3) (1 -3) (2 0)
	       (2 -5) (2 -3)
	       (2 nil) (1 -5) (1 -5)
	       (2 -3) (2 nil)
	       (4 nil)
	       (4 nil)))))

(defparameter *deda-meda-l*
  `(,@(rep 2 '((4 nil)
	       (4 nil)
	       (4 nil)
	       (4 nil)
	       (1 -7) (1 -7) (2 nil)
	       (2 nil) (1 -10) (1 -8)
	       (1 -7) (1 -7) (2 -8)
	       (4 -10)))))

;;; (gen-sound 32 1 (scan-notes (muld 1/2 *deda-meda-d*) #'violin) (scan-notes (muld 1/2 *deda-meda-l*) #'piano))