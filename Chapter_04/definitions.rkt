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

; ------------------------------
(define pow                   ; text uses typographic up-arrow
  (lambda (n m)               ; 'expt' is the defined procedure for this same function
    (cond
      ((zero? m) 1)
      (else (times n (pow n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define divide                ; text uses typographic division symbol
  (lambda (n m)               ; 'quotient' is the defined procedure for this same function
    (cond
      ((lt? n m) 0)
      (else (add1 (divide (o- n m) m))))))
; ------------------------------

; ------------------------------
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
; ------------------------------

; ------------------------------
(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
; ------------------------------

; ------------------------------
(define rempick
  (lambda (n lat)
    (cond
      ((eq? n 1) (cdr lat))
      (else (cons (car lat)(rempick (sub1 n) (cdr lat)))))))
; ------------------------------

; ------------------------------
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat)'())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
; ------------------------------

; ------------------------------
; ------------------------------

(define list1
  (list 'hotdogs 'with 'mustard 'sauerkraut 'and 'pickles))

(define list2
  (list 'lasagna 'spaghetti 'ravioli 'macaroni 'meatball))

(define list3
  (list 'hotdogs 'with 'hot 'mustard))

(define list4
  (list 5 'pears 6 'prunes 9 'dates))