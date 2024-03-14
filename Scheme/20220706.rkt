#lang racket

;Consider the technique “closures as objects” as seen in class, where a closure assumes the role of a class.
;In this technique, the called procedure (which works like a class in OOP) returns a closure which is
;essentially the dispatcher of the object.
;Define the define-dispatcher macro for generating the dispatcher in an automatic way, as illustrated by
;the following example:
(define (make-man)
 (let ((p (make-entity))
       (name "man"))
   (define prefix+name
     (lambda (prefix)
       (string-append prefix name)))
   (define change-name
     (lambda (new-name)
       (set! name new-name)))
   (define-dispatcher methods: (prefix+name change-name) parent: p)))

;where p is the parent of the current instance of class man, and make-entity is its constructor.
;If there is no inheritance (or it is a base class), define-dispatcher can be used without the parent: p part

(define-syntax define-dispatcher
  (syntax-rules (methods: parent:)
    ((_ methods: (func ...) parent: p)
     (lambda (message . args)
       (case message
         ((func) (apply func args))
         ...
         (else (apply p (cons message args))))))
    ((_ methods: func)
     (define-dispatcher methods: func parent: unknown))))

(define unknown
  (lambda ()
    (display "Unknown Method")))

(define (make-entity)
  (let ((age 0))
    (define (set-age x)
      (lambda (x)
        (set! age x)))
    (define-dispatcher methods: (set-age))))

(define carlo (make-man))
(carlo 'change-name "Carlo")
(carlo 'prefix+name "Mr. ")