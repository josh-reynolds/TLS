#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 9: ... and Again, and Again, and Again,...
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
