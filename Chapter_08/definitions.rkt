#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 8: Lambda the Ultimate
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l)
                  (rember-f test? a (cdr l)))))))
; ------------------------------


(define list1
  (list 6 2 5 3))

(define list2
  (list 'jelly 'beans 'are 'good))

(define list3
  (list 'lemonade
        (list 'pop 'corn)
        'and
        (list 'cake)))