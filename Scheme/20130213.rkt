#lang racket


#|1) Define a portion of a set library for Scheme, optimized for lookup (procedure in?) and insertion
(procedure put!) of elements. Set elements may be numbers or symbols.|#

(library (sets)
 (export
  make-set
  in?
  put!
  remove!
  intersect)
 (import (rnrs)(rnrs hashtables))

 (define make-set make-eqv-hashtable)

 (define (in? set x)
 (hashtable-ref set x #f)) ;; or hashtable-contains?

 (define (put! set x)
   (hashtable-set! set x #t))

 (define remove! hashtable-delete!)

#|2) Define an intersection procedure for this library that can have any number of sets as arguments (at
least one).|#

 (define (isect x y)
   (let ((res (hashtable-copy x #t)))
     (vector-for-each (lambda (e)
                        (remove! res e))
                      (hashtable-keys y))
     res))

 (define (intersect set . sets)
   (if (null? sets)
       set
       (intersect (isect set (car sets)) . (cdr sets))))