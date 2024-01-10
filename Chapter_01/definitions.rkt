#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 1: Toys

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

(define la
  (list 'a 'b 'c))

(define lb
  (list la 'x 'y 'z))

(define lc
  (list
   (list
    (list 'hotdogs))
   (list 'and)
   (list 'pickle)
   'relish))

(define ld
  (list
   (list
    (list 'hotdogs))
   (list 'and)))

(define le
  (list 'hamburger))

(define lf
  (list
   (list 'b)
   (list 'x 'y)
   (list
    (list 'c))))

(define lg
  (list 'butter 'and 'jelly))

(define lh
  (list 'a 'b (list 'c)))

(define li
  '())
