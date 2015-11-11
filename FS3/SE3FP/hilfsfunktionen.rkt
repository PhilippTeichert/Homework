#lang racket

;;;; Rekursives konkatenieren gleichindizierter Elemente zweier Listen
; require length(lst1) == length(lst2)
(define (zip lst1 lst2)
  (if (or (empty? lst1)
          (empty? lst2))
      '()
      (cons (cons (car lst1)
                  (car lst2)) ; end first pair
            (zip (cdr lst1)
                 (cdr lst2)) ;end remaining pairs
            ) ; end concatenation
      ) ; end if
  ) ; end define (zip lst1 lst2)