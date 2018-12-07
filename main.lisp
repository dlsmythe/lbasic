(declaim (optimize (speed 0) (space 0) (debug 3)))

(ql:quickload "lispbuilder-lexer" :verbose nil :prompt nil)
(ql:quickload "lispbuilder-yacc" :verbose nil :prompt nil)
(ql:quickload "split-sequence" :verbose nil :prompt nil)
(ql:quickload "cl-ppcre" :verbose nil :prompt nil)

(defpackage :lbasic
  (:use :cl :lispbuilder-lexer :lispbuilder-yacc :split-sequence))

(in-package :lbasic)

(define-condition basic-error (error)
  ((text :initarg :text :initform "error" :reader text)
   (statement :initarg :statement :initform nil :reader statement))
  (:report (lambda (condition stream)
	     (format stream "~a: ~a"
		     (if (statement condition) (basic-statement-lineno (statement condition)) "error")
		     (text condition)))))

(load "~/lisp/BASIC-64/lexer.lisp")
(load "~/lisp/BASIC-64/grammar.lisp")
(load "~/lisp/BASIC-64/runtime.lisp")
;;(load "~/lisp/BASIC-64/tests.lisp")
