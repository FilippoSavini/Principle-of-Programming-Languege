#lang racket

;a) Define an iterator for lists in Scheme, such that calling it returns an element. When there are no more elements, it returns
;symbol <<end>>.
;E.g.
;(define il (make-iter '(1 2)))
;(il) returns 1
;(il) returns 2
;(il) returns <<end>>

(define (iter-list lst)
  (let ((cur lst))
    (lambda ()
      (if (null? cur)
          '<<end>>
          (let ((v (car cur)))
            (set! cur (cdr cur))
            v)))))

(define my-iterator (iter-list '(1 2 3 4 5)))

(display (my-iterator)) ; Output: 1
(display (my-iterator)) ; Output: 2
(display (my-iterator)) ; Output: 3
(display (my-iterator)) ; Output: 4
(display (my-iterator)) ; Output: 5
(display (my-iterator)) ; Output: <<end>>

;b) Define an analogous iterator for vectors.

(define (iter-vec vec)
  (let ((cur 0)
        (top (vector-length vec)))
    (lambda ()
      (if (= cur top)
          '<<end>>
          (let((v (vector-ref vec cur)))
            (set! cur (+ cur 1))
            v)))))

(define snd-iter (iter-vec #(1 2 3 4)))

(display (snd-iter))
(display (snd-iter))
(display (snd-iter))
(display (snd-iter))
(display (snd-iter))

    
;c) Define a new construct "for/in" which iterates on lists or vectors.
;E.g.
;(for x in '(1 2 3) (display x)) shows 123
;(for x in '#(c a s a) (display x)(display ".")) shows c.a.s.a.



(define (iter-for block data iter)
  (let ((iterandum (iter data)))
    (let loop ((x (iterandum)))
      (if (not (eq? x '<<end>>))
          (begin
            (block x)
            (loop (iterandum)))
          (display " ")))))

(define (iter-dispatch data)
  (cond ((vector? data) iter-vec)
        ((list? data) iter-list)
        (else (display "wrong type"))))

(define-syntax for
 (syntax-rules (in)
   ((_ var in data expr ...)
    (iter-for (lambda (var) expr ...)
              data (iter-dispatch data)))))

(for x in '(1 2 3) (display x))
(for x in '#(c a s a) (display x)(display "."))
