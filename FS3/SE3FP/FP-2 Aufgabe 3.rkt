#lang racket

;Aufgabe 3
;n ist die Eingabe, von welcher der Typ bestimmt werden soll.
(define (type-of n)
  (if (boolean? n)
      "boolean"
      (if (number? n)
          "number"
          (if (char? n)
              "char"
              (if (symbol? n)
                  "symbol"
                  (if (string? n)
                      "string"
                      (if (vector? n)
                          "vector"
                          (if (and (pair? n) (<= (length n) 2))
                              "pair"
                              (if (list? n)
                                  "list"
                                  (if (procedure? n)
                                      "procedure"
                                      "ende"))))))))))

;Methode, die komischerweise nicht funktioniert O.o
(define (type-of2 n)
  (when (boolean? n)
    "boolean")
  (when (pair? n)
    "pair")
  (when (list? n)
    "list")
  (when (symbol? n)
    "symbol")
  (when (number? n)
    "number")
  (when (char? n)
    "char")
  (when (string? n)
    "string")
  (when (vector? n)
    "vector")
  (when (procedure? n)
    "procedure")
  "ende")