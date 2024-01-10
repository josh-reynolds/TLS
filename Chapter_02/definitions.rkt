#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 2: Do It, Do It Again, and Again, and Again...
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
; ------------------------------

; ------------------------------
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
; ------------------------------

(define list1
  (list 'Jack 'Sprat 'could 'eat 'no 'chicken 'fat))

(define list2
  (list 'Jack (list 'Sprat 'could) 'eat 'no 'chicken 'fat))

(define list3
  (list 'bacon 'and 'eggs))

(define list4
  (list 'bacon
        (list 'and 'eggs)))

(define list5
  (list 'mashed 'potatoes 'and 'meat 'gravy))

(define list6
  (list 'bagels 'and 'lox))