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

; ------------------------------
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
; ------------------------------

; ------------------------------
(define eq?-salad
  (eq?-c 'salad))
; ------------------------------

; ------------------------------
(define rember-f2             ; rewriting to use curried function
  (lambda (test?)             ; text overwrites but I want to preserve the original
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f2 test?) a (cdr l))))))))
; ------------------------------

; ------------------------------
(define rember-eq? (rember-f2 eq?))
; ------------------------------

; ------------------------------
(define insertL-f             ; interactions pane will lowercase to 'insertl-f'
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons new (cons (car l) (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))
; ------------------------------

; ------------------------------
(define insertR-f             ; interactions pane will lowercase to 'insertl-f'
  (lambda (test?)
    (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? old (car l)) (cons (car l) (cons new (cdr l))))
      (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))
; ------------------------------

; ------------------------------
(define seqL                  ; interactions pane will lowercase to seql
  (lambda (new old l)
    (cons new (cons old l))))
; ------------------------------

; ------------------------------
(define seqR                  ; interactions pane will lowercase to seqr
  (lambda (new old l)
    (cons old (cons new l))))
; ------------------------------

; ------------------------------
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l)) (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))
; ------------------------------

; ------------------------------
(define insertL               ; redefining from earlier chapters to use seq
  (insert-g seqL))            ; interactions pane will lowercase to 'insertl'
; ------------------------------

; ------------------------------
(define insertR               ; redefining from earlier chapters to use seq
  (insert-g seqR))            ; interactions pane will lowercase to 'insertr'
; ------------------------------

; ------------------------------
(define insertL2              ; redefining above to not pass in seqL
  (insert-g                   ; interactions pane will lowercase to 'insertl2'
   (lambda (new old l)
     (cons new (cons old l)))))
; ------------------------------

; ------------------------------
(define insertR2              ; redefining above to not pass in seqR
  (insert-g                   ; interactions pane will lowercase to 'insertr2'
   (lambda (new old l)
     (cons old (cons new l)))))
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

(define list4
  (list 'tuna 'salad 'is 'good))

(define list5
  (list 'shrimp 'salad 'and 'tuna 'salad))