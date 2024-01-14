#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 5: *Oh My Gawd*: It's Full of Stars
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

