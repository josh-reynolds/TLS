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
(define third
  (lambda (l)
    (car (cdr (cdr l)))))
; ------------------------------

; ------------------------------
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))
; ------------------------------

; ------------------------------
(define fun?
  (lambda (rel)
    (set? (firsts rel))))
; ------------------------------

; ------------------------------
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))
; ------------------------------

; ------------------------------
(define revpair
  (lambda (p)
    (build (second p)
           (first p))))
; ------------------------------

; ------------------------------
(define fullfun?              ; their definition differs - they assume the input
  (lambda (rel)               ; is a fun instead of a rel, so only need the second clause
    (and (fun? rel)
         (fun? (revrel rel)))))
; ------------------------------

; ------------------------------
(define fullfun2?             ; text version, assumes input is a fun
  (lambda (fun)
    (set? (seconds fun))))
; ------------------------------

; ------------------------------
(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l)))
                  (seconds (cdr l)))))))
; ------------------------------

; ------------------------------
(define one-to-one?           ; a third version of fullfun? from the text
  (lambda (fun)               ; same test as my version above, but assumes input is a fun
    (fun? (revrel fun))))
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

(define list15
  (list 'apples 'peaches 'pumpkin 'pie))

(define list16
  (list (list 'apples 'peaches)
        (list 'pumpkin 'pie)
        (list 'apples 'peaches)))

(define list17
  (list (list 'apples 'peaches)
        (list 'pumpkin 'pie)))

(define list18
  (list (list 4 3)
        (list 4 2)
        (list 7 6)
        (list 6 2)
        (list 3 4)))

(define list19
  (list (list 8 'a)
        (list 'pumpkin 'pie)
        (list 'got 'sick)))

(define list20
  (list (list 8 3)
        (list 4 2)
        (list 7 6)
        (list 6 2)
        (list 3 4)))

(define list21
  (list (list 8 3)
        (list 4 8)
        (list 7 6)
        (list 6 2)
        (list 3 4)))

(define list22
  (list (list 'grape 'raisin)
        (list 'plum 'prune)
        (list 'stewed 'prune)))

(define list23
  (list (list 'grape 'raisin)
        (list 'plum 'prune)
        (list 'stewed 'grape)))

(define list24
  (list (list 'chocolate 'chip)
        (list 'doughy 'cookie)))