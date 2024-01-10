#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 2: Do It, Do It Again, and Again, and Again...

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
