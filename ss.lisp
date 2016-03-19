;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2009 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :ss)

(defun locif (frq)
  (let ((lp (mod (round (* 12 (log (/ frq 55)))) 12)))
    (if (< lp 7) (* 7 (- 6 lp)) (+ 45 (* 7 (- lp 6))))))

(defun interval (n) (expt 2 (/ n 12)))

(defparameter pna '(0 0 1 1 10 0.6 25 0.3 100 0))

(defparameter *a* 440.0)

(defclass note ()
  ((duration :initarg :duration :reader duration)
   (key :initarg :key :reader key)))

(defclass amp-note (note)
  ((ampl :initarg :ampl :accessor ampl)))

(defclass style ()
  ((instrument :initarg :instrument :reader instrument)))

(defclass time-sound (amp-note style)
  ((sync-clk :initarg :sync-clk :reader sync-clk)
   (timebase :initarg :timebase :reader timebase)))

(defun copy-ts (ts &key (timebase (timebase ts)))
  (make-instance
   'time-sound
   :duration (duration ts)
   :key (key ts)
   :ampl (ampl ts)
   :instrument (instrument ts)
   :sync-clk (sync-clk ts)
   :timebase timebase))

(defmethod print-object ((ts time-sound) s)
  (print-unreadable-object (ts s :type t :identity t)
    (let ((*print-circle* nil))
      (format s "~@<~@{~S~^ ~}~:_~:>"
	      (timebase ts) (sync-clk ts) (duration ts)))))

(defun violin (ts sfn dfn)
  (let ((f (* *a* (interval (key ts)))))
    (clm::fm-violin
     (funcall sfn (timebase ts))
     (funcall dfn (duration ts))
     f (ampl ts)
     :amp-env pna :fm-index 2
     :degree (locif f))))

(defun piano (ts sfn dfn)
  (clm::p
   (funcall sfn (timebase ts))
   :duration (funcall dfn (duration ts))
   :keynum (+ (key ts) 57)))

(defun flute (ts sfn dfn)
  (let ((d (funcall dfn (duration ts))))
    (clm::stereo-flute
     (funcall sfn (timebase ts))
     d
     (* *a* (interval (key ts)))
     0.5 :flow-envelope '(0 0 25 1 75 1 100 0.5))))

(defun tubebell (ts sfn dfn)
  (let ((f (* *a* (interval (key ts)))))
    (clm::tubebell
     (funcall sfn (timebase ts))
     (funcall dfn (duration ts))
     f (ampl ts))))

(defun drum (ts sfn dfn)
  (clm::fm-drum
   (funcall sfn (timebase ts))
   (funcall dfn (duration ts))
   (* *a* (interval (- (key ts) 36))) (ampl ts)
   ;;pna pna
   5))

(defun gong (ts sfn dfn)
  (clm::gong
   (funcall sfn (timebase ts))
   (funcall dfn (duration ts))
   (* *a* (interval (- (key ts) 9))) (ampl ts)))

(defun maraca (ts sfn dfn)
  (clm::maraca
   (funcall sfn (timebase ts))
   (funcall dfn (duration ts))
   (ampl ts)))

(defun scan-notes (c &optional (instrument #'violin))
  (declare (optimizable-series-function))
  (let ((abstime 0.0)
	(tmpabs 0.0)
	(key nil))
    (mapping ((ce (apply
		   #'series
		   (mapcar
		    #'car
		    (mapcan
		     (lambda (d-keys)
		       (let ((d (car d-keys))
			     (f t))
			 (mapcar
			  (lambda (k)
			    (prog1
				(list (list f d k))
			      (when f (setf f nil))))
			  (cdr d-keys))))
		     c)))))
      (let ((flg (nth 0 ce))
	    (dur (nth 1 ce))
	    (key2 (nth 2 ce)))
	(let ((pause (null key2))
	      (abs (if flg abstime tmpabs)))
	  (unless pause
	    (setq key key2)) ; let's remember it...
	  (prog1
	      (make-instance
	       'time-sound
	       :sync-clk (and (not pause) key t)
	       :timebase abs
	       :duration dur
	       :key (or key 0)
	       :ampl .2
	       :instrument instrument)
	    (when flg
	      (shiftf tmpabs abstime (+ abstime dur)))))))))

(defmacro join-channels (&rest cs)
  (let ((bdgs (mapcar (lambda (c) `(,(gensym) ,c)) cs)))
    `(mapping ,bdgs (list ,@(mapcar #'car bdgs)))))

(defun all-null-p (&rest cs)
  (loop for c in cs always (null c)))

(defmacro timebase-limit-list (maxtb &rest cs)
  (let ((pr (gensym))
	(bdgs (mapcar (lambda (c) `(,(gensym) ,c)) cs)))
    (let ((bdgs2 (mapcar (lambda (b) `(,(gensym) ,(car b))) bdgs)))
      (let ((vars (loop for b on (cons 1 bdgs) collect (gensym))))
	`(let ((,pr (lambda (ts) (>= (timebase ts) ,maxtb))))
	   (let ,(mapcar
		  (lambda (b)
		    `(,(car b)
		       (catenate (until-if ,pr ,(cadr b)) (series nil))))
		  bdgs)
	     (multiple-value-bind ,vars
		 (until-if
		  #'identity
		  (mapping ,bdgs2 (all-null-p ,@(mapcar #'car bdgs2)))
		  ,@(mapcar #'car bdgs))
	       (declare (ignore ,(car vars)))
	       (collecting-fn
		'list
		(lambda () nil)
		(lambda (l ,@vars)
		  (declare (ignore l ,(car vars)))
		  (list ,@(cdr vars)))
		,@vars))))))))

(defmacro gen-sound (maxtb af &rest cs)
  `(clm::with-sound (:srate 44100 :play nil :statistics t)
     (producing ((res 0))
       ((l (timebase-limit-list ,maxtb ,@cs)) le)
       (loop
	  (tagbody
	     (setq le (next-in l (terminate-producing)))
	     (loop for ts in le and i from 0
		when (and ts (sync-clk ts)) do
		  (setf (ampl ts) (* (ampl ts) ,af))
		  (format t "~&~D ~D ~D ~D" i
			  (timebase ts) (duration ts) (key ts))
		  (incf res)
		  (funcall
		   (instrument ts)
		   ts
		   #'identity
		   #'identity)))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun rep (n l) (loop for i below n append l))

  (defun chord (l &optional (b -4) (a 3))
    (mapcar
     (lambda (p)
       (let ((k (cadr p)))
	 (if (and k (null (cddr p)))
	     (cons (car p) (list (+ k b) k (+ k a)))
	     p)))
     l))

  (defun muld (f l)
    (mapcar (lambda (p) (cons (* f (car p)) (cdr p))) l))

  (defun addk (s l)
    (mapcar (lambda (p)
	      (cons (car p) (mapcar (lambda (k) (when k (+ s k))) (cdr p))))
	    l)))

(defun adds (s ts)
  (copy-ts ts :timebase (+ s (timebase ts))))

(defmacro erp (e)
  (let ((v (gensym)))
    `(let ((,v ,e))
       (format *terminal-io* "~&~A =>~%  ~A~%" ',e ,v)
       (force-output *terminal-io*)
       ,v)))

(defun adjust-notes (sharp flat mn-map)
  (loop for (m n) in mn-map collect
       (cond
	 ((member m sharp) (list m (1+ n)))
	 ((member m flat) (list m (1- n)))
	 (t (list m n)))))

(defmacro with-mnemonics ((&key sharp flat) seq)
  (let ((svs
	 (loop for o from 1 to 7 nconc
	      (loop for i from 1 to 7
		 for (m n) in
		   (adjust-notes
		    sharp flat
		    '((do 0) (re 2) (mi 4) (fa 5) (sol 7) (la 9) (si 11)))
		 collect (list (intern (format nil "~A~D" m o))
			       (+ (* (- o 4) 12) n))))))
    `(symbol-macrolet ,svs
       (list ,@(mapcar (lambda (x) `(list ,@x)) (eval seq))))))