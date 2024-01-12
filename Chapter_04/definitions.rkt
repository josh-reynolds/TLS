#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 4: Numbers Games
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
