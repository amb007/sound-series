;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2009 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package #:asdf)

(defsystem #:ss
  :depends-on (:series :cm)
  :components
  ((:file "ss-package")
   (:file "ss"
	  :in-order-to ((load-source-op (load-source-op "ss-package"))
			(load-op (load-op "ss-package"))
			(compile-op (load-op "ss-package"))))
   (:file "examples"
	  :in-order-to ((load-source-op (load-source-op "ss"))
			(load-op (load-op "ss"))
			(compile-op (load-op "ss"))))
   (:file "satie"
	  :in-order-to ((load-source-op (load-source-op "ss"))
			(load-op (load-op "ss"))
			(compile-op (load-op "ss"))))))