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
