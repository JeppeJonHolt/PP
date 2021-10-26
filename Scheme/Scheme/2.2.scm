#lang scheme
(define (replicate-to-length lst len)
  (replicator lst len lst))

(define (replicator lst len copylst)
  (cond
    ((null? copylst)(replicator lst len lst))
    ((= len 0)'())
    (else (cons (car copylst)(replicator lst (- len 1)(cdr copylst))))
    ))