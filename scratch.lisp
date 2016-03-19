;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2009 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(require :asdf)
(asdf:oos 'asdf:load-op :cm)
(load "../clm/all.lisp")
(in-package :clm)
(asdf:oos 'asdf:load-op :ss)
(compile-file "jcrev.ins")
(load "jcrev")
(compile-file "v.ins")
(load "v")
(compile-file "maraca.ins")
(load "maraca")
(compile-file "fmex.ins")
(load "fmex")
(compile-file "piano.ins")
(load "piano")
(compile-file "flute.ins")
(load "flute")
(series::install)
