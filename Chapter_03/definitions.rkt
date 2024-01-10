#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 3: Cons the Magnificent
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a
                          (cdr lat)))))))
; ------------------------------


(define list1
  (list 'lamb 'chops 'and 'mint 'jelly))

(define list2
  (list 'coffee 'cup 'tea 'cup 'and 'hick 'cup))

(define list3
  (list 'bacon 'lettuce 'and 'tomato))
