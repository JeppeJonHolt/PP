#lang scheme
(define (make-comparator cmp)
  (lambda (x y)
    (cond
      ((cmp x y) 1)
      ((cmp y x) -1)
      (else 0)
  )))

(define c-compare (make-comparator string<?))

(define (lt-eq-gt cmp)
  (list
     (lambda (x y)
        (< (cmp x y) 0))
     (lambda (x y)
        (= (cmp x y) 0))
     (lambda (x y)
        (> (cmp x y) 0))))