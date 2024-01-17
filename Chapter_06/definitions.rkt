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
