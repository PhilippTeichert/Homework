#lang racket

(define miau 'Plueschi)
(define katze miau)
(define tiger 'miau)

(define (welcherNameGiltWo PersonA PersonB)
  (let ((PersonA 'Sam)
        (PersonC PersonA))
    PersonC))
(define (welcherNameGiltWo2 PersonA PersonB)
  (let* ((PersonA 'Sam)
        (PersonC PersonA))
    PersonC))

(define xs1 '(o 2 3 miau katze))
(define xs2 (list miau katze))
(define xs3 (cons katze miau))

(define x2 (list miau 'Sam))
(define x3 (cons katze 'Sam))