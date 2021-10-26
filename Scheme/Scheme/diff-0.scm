#lang scheme
(define (func x)
  (* x 3))

(define derivative
  (lambda(f)
    (lambda (x) (/ (- (f (+ x 0.0005)) (f x)) 0.0005 )))
    )

(define (compare-2 f1 f2 numerical-input-list)
  (if (null? numerical-input-list)
       '()
  (cons (- (f1 (car numerical-input-list)) (f2 (car numerical-input-list)))
       (compare-2 f1 f2 (cdr numerical-input-list)))))