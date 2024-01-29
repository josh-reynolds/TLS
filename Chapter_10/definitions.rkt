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

; ------------------------------
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table ))
      (else (evcon (cdr lines) table)))))
; ------------------------------

; ------------------------------
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))
; ------------------------------

; ------------------------------
(define question-of first)
; ------------------------------

; ------------------------------
(define answer-of second)
; ------------------------------

; ------------------------------
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
; ------------------------------

; ------------------------------
(define cond-lines-of cdr)
; ------------------------------

; ------ working through example
; (*cond list1 table4)
; (evcon (cond-lines-of list1) table4)
; (evcon (list (list 'coffee 'klatsch) 'else 'party) table4)
;   (else? (question-of (car (list (list 'coffee 'klatsch) 'else 'party)))
;   (else? (question-of (list 'coffee 'klatsch))
;   (else? 'coffee)
;     (atom? 'coffee) #t
;       (eq? 'coffee 'else) #f
;   #f
;   (meaning (question-of (car (list (list 'coffee 'klatsch) 'else 'party)) table4)
;   (meaning (question-of (list 'coffee 'klatsch)) table4)
;   (meaning 'coffee table4)
;     ((expression-to-action 'coffee) 'coffee table4)
;       (atom? 'coffee) #t
;         (atom-to-action 'coffee)
;           (number? 'coffee) #f
;            ...
;           (else *identifier)
;     (*identifier 'coffee table4)
;       (lookup-in-table 'coffee table4 initial-table)
;         (null? table) #f
;         (else
;           (lookup-in-entry 'coffee (car table4) ANON-1)
;           (lookup-in-entry 'coffee (list (list 'coffee) (list #t)) ANON-1)
;             (lookup-in-entry-help 'coffee (first (list (list 'coffee) (list #t)) (second (list (list 'coffee) (list #t)))) ANON-1)
;             (lookup-in-entry-help 'coffee (list 'coffee) (list #t) ANON-1)
;               (null? (list 'coffee)) #f
;               (eq? 'coffee (car (list 'coffee))) #t
;                 (car (list #t))
;                 #t
;   #t
;     (meaning (answer-of (car (list (list 'coffee 'klatsch) 'else 'party)) table4)
;     (meaning (answer-of (list 'coffee 'klatsch)) table4)
;     (meaning 'klatsch table4)
;       ((expression-to-action 'klatsch) 'klatsch table4)
;         (atom? 'klatsch) #t
;           (atom-to-action 'klatsch)
;             (number? 'klatsch) #f
;              ...
;             (else *identifier)
;       (*identifier 'klatsch table4)
;         (lookup-in-table 'klatsch table4 initial-table)
;           (null? table4) #f
;           (else
;             (lookup-in-entry 'klatsch (car table4) ANON-2)
;             (lookup-in-entry 'klatsch (list (list 'coffee) (list #t)) ANON-2)
;               (lookup-in-entry-help 'klatsch (first (list (list 'coffee) (list #t))) (second (list (list 'coffee) (list #t))) ANON-2)
;               (lookup-in-entry-help 'klatsch (list 'coffee) (list #t) ANON-2)
;                 (null? (list 'coffee)) #f
;                 (eq? 'klatsch (car (list 'coffee))) #f
;                 (else
;                   (lookup-in-entry-help 'klatsch (cdr (list 'coffee)) (cdr (list #t)) ANON-2)
;                   (lookup-in-entry-help 'klatsch '() '() ANON-2)
;                     (null? '()) #t
;                       (ANON-2 'klatsch)
;                       (lookup-in-table 'klatsch (cdr table4) initial-table)
;                       (lookup-in-table 'klatsch (list (list (list 'klatsch 'party) (list 5 (list 6)))) initial-table)
;                         (null? (list (list (list 'klatsch 'party) (list 5 (list 6))))) #f
;                         (else
;                           (lookup-in-entry 'klatsch (car (list (list (list 'klatsch 'party) (list 5 (list 6))))) ANON-3)
;                           (lookup-in-entry 'klatsch (list (list 'klatsch 'party) (list 5 (list 6))) ANON-3)
;                             (lookup-in-entry-help 'klatsch (first ENTRY) (second ENTRY) ANON-3)
;                             (lookup-in-entry-help 'klatsch (list 'klatsch 'party) (list 5 (list 6)) ANON-3)
;                               (null? (list 'klatsch 'party)) #f
;                               (eq? 'klatsch (car (list 'klatsch 'party)))
;                               (eq? 'klatsch 'klatsch) #t
;                                 (car (list 5 (list 6)))
;                                 5
; 5
; ------------------------------

; ------------------------------
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else (cons (meaning (car args) table) (evlis (cdr args) table))))))
; ------------------------------

; ------------------------------
(define *application          ; text uses 'apply' but there is already a built-in function with that name
  (lambda (e table)           ; using 'applie' instead
    (applie 
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))
; ------------------------------

; ------------------------------
(define function-of car)
; ------------------------------

; ------------------------------
(define arguments-of cdr)
; ------------------------------

; ------------------------------
(define applie
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))       
; ------------------------------

; ------------------------------
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
; ------------------------------

; ------------------------------
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))
; ------------------------------

; ------------------------------
(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons)) (cons (first vals) (second vals)))
      ((eq? name (quote car)) (car (first vals)))
      ((eq? name (quote cdr)) (cdr (first vals)))
      ((eq? name (quote null?)) (null? (first vals)))
      ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
      ((eq? name (quote atom?)) (:atom? (first vals)))
      ((eq? name (quote zero?)) (zero? (first vals)))
      ((eq? name (quote add1)) (add1 (first vals)))
      ((eq? name (quote sub1)) (sub1 (first vals)))
      ((eq? name (quote number?)) (number? (first vals))))))
; ------------------------------

; ------------------------------
(define add1
  (lambda (n)
    (+ n 1)))
; ------------------------------

; ------------------------------
(define sub1
  (lambda (n)
    (- n 1)))
; ------------------------------

; ------------------------------
(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive)) #t)
      ((eq? (car x) (quote non-primitive)) #t)
      (else #f))))
; ------------------------------

; ------------------------------
(define apply-closure 'APPLY-CLOSURE)
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

(define table3
  (list (list (list 'y 'z)
              (list (list 8) 9))))

(define list1
  (list 'cond
        (list 'coffee
              'klatsch)
        'else
        'party))

(define table4
  (list (list (list 'coffee)
              (list #t))
        (list (list 'klatsch 'party)
              (list 5
                    (list 6)))))
              