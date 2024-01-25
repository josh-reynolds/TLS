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
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))
; ------------------------------

; ------------------------------
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))
; ------------------------------

; ------------------------------
(define first
  (lambda (l)
    (car l)))
; ------------------------------

; ------------------------------
(define second
  (lambda (l)
    (car (cdr l))))
; ------------------------------

; ------------------------------
(define extend-table cons)
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

(define table1
  (list entry1
        entry3))

(define table2
  (list (list (list 'entrée 'dessert)
              (list 'spaghetti 'spumoni))
        (list (list 'appetizer 'entrée 'beverage)
              (list 'food 'tastes 'good))))