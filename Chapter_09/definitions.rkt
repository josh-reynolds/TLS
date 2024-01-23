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

; ------------------------------
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
; ------------------------------

; ------------------------------
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))
; ------------------------------

; ------------------------------
(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
; ------------------------------

; ------------------------------
(define sub1
  (lambda (n)
    (- n 1)))
; ------------------------------

;; working out the example -----
;(looking 'caviar list1)
;  (keep-looking 'caviar (pick 1 list1) list1)
;  (keep-looking 'caviar 6 list1)
;    ((number? 6) #t
;      (keep-looking 'caviar (pick 6 list1) list1)
;      (keep-looking 'caviar 7 list1)
;        ((number? 7) #t
;          (keep-looking 'caviar (pick 7 list1) list1)
;          (keep-looking 'caviar 3 list1)
;            ((number? 3) #t
;              (keep-looking 'caviar (pick 3 list1) list1)
;              (keep-looking 'caviar 4 list1)
;                ((number? 4) #t
;                  (keep-looking 'caviar (pick 4 list1) list1)
;                  (keep-looking 'caviar 'caviar list1)
;                    ((number? 'caviar) #f
;                    (else (eq? 'caviar 'caviar))
;                           #t
; #t
; ------------------------------

; ------------------------------
(define eternity              ; infinite recursion !!!
  (lambda (x)
    (eternity x)))
; ------------------------------

; ------------------------------
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
; ------------------------------

; ------------------------------
(define first
  (lambda (p)
    (car p)))
; ------------------------------

; ------------------------------
(define second
  (lambda (p)
    (car (cdr p))))
; ------------------------------

; ------------------------------
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
; ------------------------------

; ------------------------------
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
; ------------------------------

; ------------------------------
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))
; ------------------------------

; ------------------------------
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (length* (first pora))
                (length* (second pora)))))))
; ------------------------------

; ------------------------------
(define add1
  (lambda (n)
    (+ n 1)))
; ------------------------------

; ------------------------------
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
; ------------------------------
     
(define list1
  (list 6 2 4 'caviar 5 7 3))

(define list2
  (list 6 2 'grits 'caviar 5 7 3))

(define list3
  (list (list 'a 'b) 'c))

(define list4
  (list (list 'a 'b)
        (list 'c 'd)))



