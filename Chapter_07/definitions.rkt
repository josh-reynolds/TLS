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

; ------------------------------
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))
; ------------------------------

; ------------------------------
(define member?               ; rewritten to use o-equal? instead of eq?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((o-equal? a (car lat)) #t)
      (else (member? a (cdr lat))))))
; ------------------------------

; ------------------------------
(define o-equal?              ; the text uses 'equal?' but this (and 'eq?)
  (lambda (s1 s2)             ; is already defined
    (cond
      ((and (atom? s1)(atom? s2)) (equan? s1 s2))
      ((or (atom? s1)(atom? s2)) #f)
      (else (eqlist? s1 s2)))))
; ------------------------------

; ------------------------------
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2)) #t)
      ((or (null? l1)(null? l2)) #f)
      (else (and (o-equal? (car l1)(car l2))
                 (o-equal? (cdr l1)(cdr l2)))))))
; ------------------------------

; ------------------------------
(define equan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)(number? a2))(= a1 a2))
      ((or (number? a1)(number? a2)) #f)
      (else (eq? a1 a2)))))
; ------------------------------

; ------------------------------
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))
; ------------------------------

; ------------------------------
(define makeset2              ; alternate version using multirember
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)(makeset2 (multirember (car lat)(cdr lat))))))))
; ------------------------------

; ------------------------------
(define multirember           ; rewritten to use o-equal? instead of eq?
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((o-equal? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
; ------------------------------

; ------------------------------
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))
; ------------------------------

; ------------------------------
(define subset2?              ; alternate version using 'and'
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset2? (cdr set1) set2))))))
; ------------------------------

; ------------------------------
(define eqset?
  (lambda (set1 set2)
    (and (subset2? set1 set2)
         (subset2? set2 set1))))
; ------------------------------

; ------------------------------
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))
; ------------------------------

; ------------------------------
(define intersect2?           ; alternate version using 'or'
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect2? (cdr set1) set2))))))
; ------------------------------

; ------------------------------
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
; ------------------------------

; ------------------------------
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))
; ------------------------------

; ------------------------------
(define setdiff               ; in the text they name this one 'xxx'
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (setdiff (cdr set1) set2))
      (else (cons (car set1) (setdiff (cdr set1) set2))))))
; ------------------------------

; ------------------------------
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))
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

(define list1
  (list 'apple 'peaches 'apple 'plum))

(define list2
  (list 'apples 'peaches 'pears 'plums))

(define list3
  (list 'apple 3 'pear 4 9 'apple 3 4))

(define list4
  (list 'apple 3 'pear 4))

(define list5
  (list 'apple 'peach 'pear 'peach 'plum 'apple 'lemon 'peach))

(define list6
  (list 5 'chicken 'wings))

(define list7
  (list 5 'hamburgers 2 'pieces 'fried 'chicken 'and 'light 'duckling 'wings))

(define list8
  (list 6 'large 'chickens 'with 'wings))

(define list9
  (list 6 'chickens 'with 'large 'wings))

(define list10
  (list 'stewed 'tomatoes 'and 'macaroni))

(define list11
  (list 'macaroni 'and 'cheese))

(define list12
  (list 'stewed 'tomatoes 'and 'macaroni 'casserole))

(define list13
  (list (list 'a 'b 'c)
        (list 'c 'a 'd 'e)
        (list 'e 'f 'g 'h 'a 'b)))

(define list14
  (list (list 6 'pears 'and)
        (list 3 'peaches 'and 6 'peppers)
        (list 8 'pears 'and 6 'plums)
        (list 'and 6 'prunes 'with 'some 'apples)))