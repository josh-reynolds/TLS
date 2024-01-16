#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 5: *Oh My Gawd*: It's Full of Stars
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l))(rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l))(rember* a (cdr l)))))))
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
(define insertR*              ; this will be lowercased to 'insertr*'
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l)(insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))(insertR* new old (cdr l)))))))
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
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l))(add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))
; ------------------------------

; ------------------------------
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))(cons new (subst* new old (cdr l))))
         (else (cons (car l)(subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))
; ------------------------------

; ------------------------------
(define insertL*              ; this will be lowercased to 'insertl*'
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l)(insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))(insertL* new old (cdr l)))))))
; ------------------------------

; ------------------------------
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? a (car l))
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))
; ------------------------------

; ------------------------------
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
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
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2)) #t)
      ((or (null? l1)(null? l2)) #f)
      (else (and (o-equal? (car l1)(car l2))
                 (o-equal? (cdr l1)(cdr l2)))))))
; ------------------------------

; ------------------------------
(define o-equal?              ; the text uses 'equal?' but this (and 'eq?)
  (lambda (s1 s2)             ; is already defined
    (cond
      ((and (atom? s1)(atom? s2)) (equan? s1 s2))
      ((or (atom? s1)(atom? s2)) #f)
      (else (eqlist? s1 s2)))))
; ------------------------------

(define list1
  (list (list 'coffee)
        'cup
        (list (list 'tea)
              'cup)
        (list 'and
              (list 'hick))
        'cup))

(define list2
  (list (list (list 'tomato
                    'sauce)
              (list (list 'bean)
                    'sauce)
              (list 'and
                    (list (list 'flying)
                          'sauce)))))

(define list3
  (list (list 'how 'much (list 'wood))
        'could
        (list (list 'a
                    (list 'wood)
                    'chuck))
        (list (list (list 'chuck)))
        (list 'if
              (list 'a)
              (list
               (list 'wood 'chuck)))
        'could 'chuck 'wood))

(define list4
  (list (list 'banana)
        (list 'split (list (list (list (list 'banana 'ice)))
                           (list 'cream (list 'banana))
                           'sherbet))
        (list 'banana)
        (list 'bread)
        (list 'banana 'brandy)))

(define list5
  (list (list 'potato)
        (list 'chips
              (list (list 'with)
                    'fish))
        (list 'chips)))

(define list6
  (list (list (list 'hot)
              (list 'tuna
                    (list 'and)))
        'cheese))

(define list7
  (list 'strawberry 'ice 'cream))

(define list8
  (list 'strawberry 'ice 'cream))