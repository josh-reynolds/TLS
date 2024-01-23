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

(define list1
  (list 6 2 4 'caviar 5 7 3))

(define list2
  (list 6 2 'grits 'caviar 5 7 3))





