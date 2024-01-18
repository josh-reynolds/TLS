#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 6: Shadows
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define numbered?             ; the text uses typographic times & up-arrow symbols
  (lambda (aexp)              ; I'm replacing with 'x' and '^' respectively
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))
; ------------------------------

; ------------------------------
(define value                 ; infix + prefix versions merged together via help functions, below
  (lambda (nexp)              
    (cond                     
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (o+ (value (1st-sub-exp nexp))
                                    (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x) (times (value (1st-sub-exp nexp))
                                       (value (2nd-sub-exp nexp))))
      (else (pow (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))
; ------------------------------

; ------------------------------
;(define 1st-sub-exp          ; for prefix notation (+ n m)
;  (lambda (aexp)
;    (car (cdr aexp))))
; ------------------------------

; ------------------------------
;(define operator             ; for prefix notation (+ n m)
;  (lambda (aexp)
;    (car aexp)))
; ------------------------------

; ------------------------------
(define 1st-sub-exp           ; for infix notation (n + m)
  (lambda (aexp)
    (car aexp)))
; ------------------------------

; ------------------------------
(define operator              ; for infix notation (n + m)
  (lambda (aexp)
    (car (cdr aexp))))
; ------------------------------

; ------------------------------
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
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
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define times                 ; text uses typographic times symbol, but 'x' is easily confused
  (lambda (n m)               ; with a variable, and '*' is a primitive operation...
    (cond
      ((zero? m) 0)
      (else (o+ n (times n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define pow                   ; text uses typographic up-arrow
  (lambda (n m)               ; 'expt' is the defined procedure for this same function
    (cond
      ((zero? m) 1)
      (else (times n (pow n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define sero?                 ; for 'parentheses representation' (), (()), (()()), (()()()),...
  (lambda (n)
    (null? n)))
; ------------------------------

; ------------------------------
(define edd1                  ; for 'parentheses representation' (), (()), (()()), (()()()),...
  (lambda (n)
    (cons '() n)))
; ------------------------------

; ------------------------------
(define zub1                  ; for 'parentheses representation' (), (()), (()()), (()()()),...
  (lambda (n)
    (cdr n)))
; ------------------------------

; ------------------------------
(define o++                   ; for 'parentheses representation' (), (()), (()()), (()()()),...
  (lambda (n m)               ; text overwrites the previous definition, but I want to keep both
    (cond
      ((sero? m) n)
      (else (edd1 (o++ n (zub1 m)))))))
; ------------------------------

; ------------------------------
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
; ------------------------------

(define list1
  (list '1 '+ (list '3 '^ '4)))

(define list2
  (list 4 '+ 3))

(define list3
  (list '+
        (list 'x 3 6)
        (list '^ 8 2)))

(define list4
  (list '+ 1 3))

(define sero '())             ; for 'parentheses representation' (), (()), (()()), (()()()),...
(define wun (edd1 sero))
(define tiw (edd1 (edd1 sero)))
(define tre (edd1 (edd1 (edd1 sero))))