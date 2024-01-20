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

; ------------------------------
(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l))(cons new (cdr l)))
      (else (cons (car l)(subst new old (cdr l)))))))
; ------------------------------

; ------------------------------
(define seqS                  ; interactions pane will lowercase to seqr
  (lambda (new old l)
    (cons new l)))
; ------------------------------

; ------------------------------
(define subst2                ; redefining to use insert-g
  (insert-g seqS))
; ------------------------------

; ------------------------------
(define seqrem
  (lambda (new old l) l))
; ------------------------------

; ------------------------------
(define rember2               ; text names this yyy, but it is another version of rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
; ------------------------------

;; working through example from text -------------------
;(rember2 'sausage list6)
;((insert-g seqrem) #f 'sausage list6)
;((null? list6) #f)
;((eq? 'sausage 'pizza) #f)
;(cons 'pizza ((insert-g seqrem) #f 'sausage (list 'with 'sausage 'and 'bacon)))
;             ((null? (list 'with 'sausage 'and 'bacon)) #f)
;             ((eq? 'sausage 'with) #f)
;             (cons 'with ((insert-g seqrem) #f 'sausage (list 'sausage 'and 'bacon)))
;                         ((null? (list 'sausage 'and 'bacon)) #f)
;                         ((eq? 'sausage 'sausage) #t)(seqrem #f 'sausage (list 'and 'bacon))
;                                                     (list 'and 'bacon)
;             (list 'with 'and 'bacon)
;(list 'pizza 'with 'and 'bacon)
; ------------------------------

; ------------------------------
(define value
  (lambda (nexp)              
    (cond                     
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (o+ (value (1st-sub-exp nexp))
                                    (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x) (times (value (1st-sub-exp nexp))
                                       (value (2nd-sub-exp nexp))))
      (else (pow (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))
; ------------------------------

; ------------------------------
(define 1st-sub-exp          ; for prefix notation (+ n m)
  (lambda (aexp)
    (car (cdr aexp))))
; ------------------------------

; ------------------------------
(define operator             ; for prefix notation (+ n m)
  (lambda (aexp)
    (car aexp)))
; ------------------------------

; ------------------------------
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
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
(define times                 ; text uses typographic times symbol, but 'x' is easily confused
  (lambda (n m)               ; with a variable, and '*' is a primitive operation...
    (cond
      ((zero? m) 0)
      (else (o+ n (times n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define pow                   ; text uses typographic up-arrow
  (lambda (n m)               ; 'expt' is the defined procedure for this same function
    (cond
      ((zero? m) 1)
      (else (times n (pow n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define atom-to-function      ; variable name and times symbol mildly confusing here...
  (lambda (x)
    (cond
      ((eq? x '+) o+)
      ((eq? x 'x) times)
      (else pow))))
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

(define list6
  (list 'pizza 'with 'sausage 'and 'bacon))