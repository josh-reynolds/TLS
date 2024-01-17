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
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (o+ (value (car nexp))
                                         (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x) (times (value (car nexp))
                                        (value (car (cdr (cdr nexp))))))
      (else (pow (value (car nexp))
                 (value (car (cdr (cdr nexp)))))))))
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

(define list1
  (list '1 '+ (list '3 '^ '4)))

(define list2
  (list 4 '+ 3))