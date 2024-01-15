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
        