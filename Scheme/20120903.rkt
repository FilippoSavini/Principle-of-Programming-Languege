#lang racket

#|Let us consider trees memorized in Scheme as hierarchical lists (e.g. numeric expressions like (+ 2 3 (- 4 5)
(/ 3 2))).|#

#|1. Define a short, purely functional version of procedure numnodes, that accepts a tree and returns its
number of nodes (e.g. (numnodes '(+ 2 3 (/ 1 3) (- 2 2 4 -7)))) should return 11).|#

(define (numnodes tree)
  (if (not(list? tree))
      1
      (+ 1 (apply + (map numnodes (cdr tree))))))

#|2. Define a lower-level, efficient, purely iterative version of numnodes.|#

(define (numnodesit tree)
  (define stack0 (list tree))
  (let loop ((stack (cdr stack0))
             (res 1)
             (cur (car stack0)))
    (when (list? cur)
        (for-each (lambda (x)
                    (set! stack (cons x stack)))
                  (cdr cur)))
    (if (null? stack)
        res
        (loop (cdr stack)
              (+ 1 res)
              (car stack)))))

(numnodesit '(+ 2 3 (/ 1 3) (- 2 2 4 -7)))

#|3. Comment the following code, giving meaningful names to capitalized elements (i.e. H1 V1 ...). Also,
please show a meaningful example usage.|#

;; The code is an iterator which returns a pair (value . continuation)

(define (H1 H2) ;; H2 is a list, H1 iterator
 (call/cc
  (lambda (V1) ;; V1 exit
    (for-each (lambda (x)
                (call/cc
                 (lambda (V2) ;; V2 continuation
                   (V1 (cons x V2)))))
              H2) 
    'V3))) ;; V3 end

(define (test)
  (let loop ((a (H1 '(1 2 3))))
    (when (not (eq? a 'V3))
      (begin
          (display (car a))
          (newline)
          (loop ((cdr a)))))))

(test)