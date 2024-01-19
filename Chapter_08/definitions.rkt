#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 8: Lambda the Ultimate
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
