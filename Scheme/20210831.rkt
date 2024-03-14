#lang racket

;;1) Define a procedure which takes a natural number n and a default value, and creates a n by n matrix
;;filled with the default value, implemented through vectors (i.e. a vector of vectors).
;;2) Let S = {0, 1, ..., n-1} x {0, 1, ..., n-1} for a natural number n. Consider a n by n matrix M, stored in a
;;vector of vectors, containing pairs (x,y) ∈ S, as a function from S to S (e.g. f(2,3) = (1,0) is represented
;;by M[2][3] = (1,0)). Define a procedure to check if M defines a bijection (i.e. a function that is both
;;injective and surjective).

(define (make-matrix n def)
  (define vec (make-vector n #f))
  (let loop ((i 0))
    (vector-set! vec i (make-vector n def))
    (when (< i (- n 1))
      (loop (+ i 1))))
  vec)


(define (bijection? m)
  (define len (vector-length m))
  (define seen? (make-matrix len #f))
  (call/cc (lambda (exit)
             (let loopi ((i 0))
               (when (< i len)
                 (let loopj ((j 0))
                   (when (< j len)
                     (let ((tmp (vector-ref (vector-ref m i) j)))
                       (if (vector-ref (vector-ref seen? (car tmp)) (car (cdr tmp)))
                           (exit #f)
                           (vector-set! (vector-ref seen? (car tmp)) (car (cdr tmp)) #t)))
                     (loopj (+ j 1))))
                 (loopi (+ i 1))))
             #t)))


(define (set-value-matrix matrix r c v)
  (vector-set! (vector-ref matrix r) c v))

(let ((my (make-matrix 2 0)))
  (begin
    (set-value-matrix my 0 0 '(1 1))
    (set-value-matrix my 0 1 '(1 0))
    (set-value-matrix my 1 0 '(0 1))
    (set-value-matrix my 1 1 '(0 0))
    (display my)
    (display (bijection? my))))