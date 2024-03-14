#lang racket
;;with lambda
(define hello-world
  (lambda ()
    (display "hello world")
    (newline)))
;;w/o lambda
(define (hello)
  (display "hello")
  (newline))

(hello-world)
(hello)


(define (minimum L)
  (let ((x (car L))
        (xs (cdr L)))
        (if (null? xs)
            x
            (minimum (cons
                      (if (< x (car xs))
                          x
                          (car xs))
                      (cdr xs))))))

(minimum '(1 2 4 7 9))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fact 3)

(define (fact-tail n)
  (define (core n acc)
    (if (= n 0)
        acc
        (core (- n 1) (* n acc))))
  (core n 1))

(fact-tail 3)

;;len tail recursive
(define (len L)
  (define (len-tail L acc)
    (if (null? L)
        acc
        (len-tail (cdr L) (+ acc 1))))
  (len-tail L 0))

(len '(1 2 3 4))

;;range python style
(define (range lo hi)
  (if (> lo hi)
      (display "error")
      (if (< lo hi)
          (cons lo (range (+ lo 1) hi))          
          (list lo))))

(display (range 2 7))
(newline)

;;range step
(define (range-step lo hi step)
  (if (> lo hi)
      (error "lo > hi")
      (if(< lo hi)
         (cons lo (range-step (+ lo step) hi step))
         '())))

(display (range-step 2 10 2))
(newline)
      
;;fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(display (fib 10))
(newline)

;;show first n fibonacci elem
(define (show-fib n)
  (define (fib-tail n acc)
    (if (= n acc)
        (display (fib acc))
        (begin
          (display (fib acc))
          (display " ")
          (fib-tail n (+ acc 1)))))
    (fib-tail n 0))

(show-fib 10)
(newline)

;;Default value with key
(define (optgreet name #:hi [hi "Hello"])
  (string-append hi ", " name))

(optgreet "Filippo" #:hi"ciao") ;;if not specified #:hi print "Hello Filippo"
(newline)

;;macro while
(define-syntax while
  (syntax-rules ()
    ((_ condition body ...)
     (let loop ()
       (when condition
         (begin
           body ...)
         (loop))))))

(define (test x)
  (let ((y x))
    (while (> 10 y)
         (set! y (+ y 1))
         (display y))))

(test 0)

        
  
        
    
