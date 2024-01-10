#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 3: Cons the Magnificent
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------