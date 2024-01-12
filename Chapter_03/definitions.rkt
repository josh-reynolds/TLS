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

; ------------------------------
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))
; ------------------------------

; ------------------------------
(define insertR           ; interactions window lowercases this as 'insertr' - watch out!
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
; ------------------------------

; ------------------------------
(define insertL           ; interactions window lowercases this as 'insertl' - watch out!
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))
; ------------------------------

; ------------------------------
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))
; ------------------------------

; ------------------------------
(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? old1 (car lat))
           (eq? old2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old1 old2 (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multiinsertR      ; interactions window lowercases this as 'multiinsertr' - watch out!
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multiinsertL      ; interactions window lowercases this as 'multiinsertl' - watch out!
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
; ------------------------------

(define list1
  (list 'lamb 'chops 'and 'mint 'jelly))

(define list2
  (list 'coffee 'cup 'tea 'cup 'and 'hick 'cup))

(define list3
  (list 'bacon 'lettuce 'and 'tomato))

(define list4
  (list 'soy 'sauce 'and 'tomato 'sauce))

(define list5
  (list (list 'a 'b)
        (list 'c 'd)
        (list 'e 'f)))

(define list6
  (list 'ice 'cream 'with 'fudge 'for 'dessert))

(define list7
  (list 'tacos 'tamales 'and 'salsa))

(define list8
  (list 'banana 'ice 'cream 'with 'chocolate 'topping))