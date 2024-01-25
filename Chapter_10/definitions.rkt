#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 10: What is the Value of All of This?
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
; ------------------------------

; ------------------------------
(define new-entry build)
; ------------------------------

; ------------------------------
(define lookup-in-entry
  (lambda (name entry)
    (...)))
; ------------------------------


(define entry1
  (list (list 'appetizer 'entrée 'beverage)
        (list 'paté 'boeuf 'vin)))

(define entry2
  (list (list 'appetizer 'entrée 'beverage)
        (list 'beer 'beer 'beer)))

(define entry3
  (list (list 'beverage 'dessert)
        (list (list 'food 'is)
              (list 'number 'one 'with 'us))))