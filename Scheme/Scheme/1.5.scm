#lang scheme
(define (every-second-element lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (car  lst))
  (else (cons(car lst) (every-second-element (cddr lst))))))
