;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2009 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(defpackage #:ss
  (:use :cl :cl-user :series :cm)
  (:shadowing-import-from :series #:scan #:split))