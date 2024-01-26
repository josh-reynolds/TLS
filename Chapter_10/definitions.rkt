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
(define third
  (lambda (l)
    (car (cdr (cdr l)))))
; ------------------------------

; ------------------------------
(define extend-table cons)
; ------------------------------

; ------------------------------
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name (cdr table) table-f)))))))
; ------------------------------

; ------------------------------
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))
; ------------------------------

; ------------------------------
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))
; ------------------------------

; ------------------------------
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))
; ------------------------------

; ------------------------------
; PLACEHOLDERS TO ALLOW EVALUATION
(define *cond 'COND)
(define *application 'APPLICATION)
; ------------------------------

; ------------------------------
(define value
  (lambda (e)
    (meaning e (quote ()))))
; ------------------------------

; ------------------------------
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
; ------------------------------

; ------------------------------
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))
; ------------------------------

; ------------------------------
(define *quote
  (lambda (e table)
    (text-of e)))
; ------------------------------

; ------------------------------
(define text-of second)
; ------------------------------

; ------------------------------
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
; ------------------------------

; ------------------------------
(define initial-table
  (lambda (name)
    (car (quote ()))))
; ------------------------------

; ------------------------------
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))
; ------------------------------

; ------------------------------
(define table-of first)
; ------------------------------

; ------------------------------
(define formals-of second)
; ------------------------------

; ------------------------------
(define body-of third)
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