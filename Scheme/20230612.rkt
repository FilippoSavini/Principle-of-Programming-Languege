#lang racket

;Write a function, called fold-left-right, that computes both fold-left and fold-right, returning them in a pair. Very
;important: the implementation must be one-pass, for efficiency reasons, i.e. it must consider each element of the
;input list only once; hence it is not correct to just call Scheme’s fold-left and -right.
;Example: (fold-left-right string-append "" '("a" "b" "c")) is the pair ("cba" . "abc")

(define (fold-left-right f acc L)
  (let loop ((accl acc)
             (accr (lambda(x) x))
             (xs L))
    (if (null? xs)
        (cons accl (accr acc))
        (loop (f (car xs) accl)
               (lambda (x)
                 (accr (f (car xs) x)))
               (cdr xs)))))



(fold-left-right string-append "" '("a" "b" "c"))

      
