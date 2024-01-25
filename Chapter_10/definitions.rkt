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
