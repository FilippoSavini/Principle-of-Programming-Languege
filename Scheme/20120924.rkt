#lang racket


#| Please define make-matrix, with three parameters: r, c, and fill. This procedure returns a matrix
having r rows and c columns, with every cell initialized with the value fill.
Hint: the standard library offers a procedure make-vector with two arguments: the first is the requested
vector size, while the second is the content we want to initialize the vector with.|#

(define (make-matrix r c fill)
  (let ((vec (make-vector r)))
    (let loop ((x 0))
      (if (< x r)
          (begin
            (vector-set! vec x (make-vector c fill))
            (loop (+ 1 x)))
          vec))))

(define test (make-matrix 2 3 1))
(display test)
(newline)

#|2. Define also the setter and the accessor, called matrix-set! and matrix-ref respectively, with the
natural parameters.|#

(define-syntax matrix-set!
  (syntax-rules ()
    ((_ matrix r c val)
     (vector-set! (vector-ref matrix (- r 1)) (- c 1) val))))

(define-syntax matrix-ref
  (syntax-rules ()
    ((_ matrix r c)
     (vector-ref (vector-ref matrix (- r 1)) (- c 1)))))

(matrix-set! test 1 2 0)
(display test)
(newline)

(display (matrix-ref test 1 2))
