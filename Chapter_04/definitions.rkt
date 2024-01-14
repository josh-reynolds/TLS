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

; ------------------------------
(define add1
  (lambda (n)
    (+ n 1)))
; ------------------------------

; ------------------------------
(define sub1
  (lambda (n)
    (- n 1)))
; ------------------------------

; ------------------------------
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup)(addtup (cdr tup)))))))
; ------------------------------

; ------------------------------
(define times                 ; text uses typographic times symbol, but 'x' is easily confused
  (lambda (n m)               ; with a variable, and '*' is a primitive operation...
    (cond
      ((zero? m) 0)
      (else (o+ n (times n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1)(car tup2))(tup+ (cdr tup1)(cdr tup2)))))))
; ------------------------------

; ------------------------------
(define gt?                   ; text uses '>' but this is already a defined primitive operation
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt? (sub1 n) (sub1 m))))))
; ------------------------------

; ------------------------------
(define lt?                   ; text uses '<' but this is already a defined primitive operation
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt? (sub1 n) (sub1 m))))))
; ------------------------------

; ------------------------------
(define equals?               ; text uses '=' but this is already a defined primitive operation
  (lambda (n m)               ; and the shorter 'eq?' is also already defined
    (cond
      ((lt? n m) #f)
      ((gt? n m) #f)
      (else #t))))
; ------------------------------