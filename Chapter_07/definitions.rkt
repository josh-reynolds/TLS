#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 7: Friends and Relations
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------