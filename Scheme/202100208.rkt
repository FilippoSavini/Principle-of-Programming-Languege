#lang racket


(define (depth-encode L)
  (define (help L level)
    (if (null? L) L
        (if (list? (car L))
            (append (help (car L) (+ level 1)) (help (cdr L) level))
            (cons (cons level (car L)) (help (cdr L) level)))))
  (help L 0))



  
(depth-encode '(1 (2 3) 4 (((5) 6 (7)) 8) 9 (((10))))) 
;returns
;((0 . 1) (1 . 2) (1 . 3) (0 . 4) (3 . 5) (2 . 6) (3 . 7) (1 . 8) (0 . 9) (3 . 10))