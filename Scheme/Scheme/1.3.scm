#lang scheme
(define (Proper-listcheck lst)
  (or (null? lst)
      (and(pair? lst)(Proper-listcheck (cdr lst)))))