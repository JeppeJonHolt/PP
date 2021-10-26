#lang scheme
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail stream) (force (cdr stream)))

(define empty-stream? null?)

(define the-empty-stream '())


(define (stream-section n stream)
  (cond ((= n 0) '())
        (else (cons (head stream)
              (stream-section 
                (- n 1)
               (tail stream))))))

(define (add-streams s1 s2)
 (let ((h1 (head s1))
       (h2 (head s2)))
   (cons-stream 
    (+ h1 h2)
    (add-streams (tail s1) (tail s2)))))

(define (mult-streams s1 s2)
  (let ((h1 (head s1))
        (h2 (head s2)))
    (cons-stream
     (* h1 h2)
     (mult-streams (tail s1) (tail s2)))))

(define ones (cons-stream 1 ones))

(define nat-nums 
 (cons-stream 1 
  (add-streams ones nat-nums)))

(define factorial
  (cons-stream 1
    (mult-streams (force(cdr nat-nums)) factorial)))