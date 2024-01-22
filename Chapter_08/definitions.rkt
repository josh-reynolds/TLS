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

; ------------------------------
(define value2                ; rewritten to use atom-to-function
  (lambda (nexp)              ; text overwrites but I want to preserve the original
    (cond                     
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp)) (value2 (1st-sub-exp nexp))
                                                (value2 (2nd-sub-exp nexp)))))))
; ------------------------------

; ------------------------------
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))
; ------------------------------

; ------------------------------
(define multirember-eq?
  (multirember-f eq?))
; ------------------------------

; ------------------------------
(define eq?-tuna
  (eq?-c 'tuna))
; ------------------------------

; ------------------------------
(define multiremberT          ; interactions pane will lowercase to multirembert
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? a (car lat)) (multirember&co a
                                         (cdr lat)
                                         (lambda (newlat seen)
                                           (col newlat
                                                (cons (car lat) seen)))))
      (else (multirember&co a
                            (cdr lat)
                            (lambda (newlat seen)
                              (col (cons (car lat) newlat)
                                   seen)))))))
; ------------------------------

; ------------------------------
(define a-friend
  (lambda (x y)
    (null? y)))
; ------------------------------

; ------------------------------
(define new-friend            ; expanding out the example problem
  (lambda (newlat seen)
    (a-friend newlat
              (cons 'tuna seen))))
; ------------------------------

; ------------------------------
(define latest-friend         ; expanding out the example problem
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))
; ------------------------------

; ------------------------------
(define another-friend        ; expanding out the example problem
  (lambda (newlat seen)
    (latest-friend newlat (cons 'tuna seen))))
; ------------------------------

; ------------------------------
(define last-friend
  (lambda (x y)
    (length x)))
; ------------------------------

;; working through examples from text -------------------
;(multirember&co 'tuna '() a-friend)
;  (null? '()) #t
;    (a-friend '() '())
;      (null? '()) #t
; #t
; ------------------------------
;(multirember&co 'tuna (list 'tuna) a-friend)
;  (null? (list 'tuna)) #f
;  (eq? 'tuna (car (list 'tuna)))
;  (eq? 'tuna 'tuna) #t
;    (multirember&co 'tuna (cdr (list 'tuna)) (lambda (newlat seen) (a-friend newlat (cons (car (list 'tuna)) seen))))
;    (multirember&co 'tuna '() (lambda (newlat seen) (a-friend newlat (cons 'tuna seen))))
;    (multirember&co 'tuna '() new-friend)
;      (null? '()) #t
;        (new-friend '() '())
;          (a-friend '() (cons 'tuna '()))
;          (a-friend '() (list 'tuna))
;            (null? (list 'tuna)) #f
; #f
; ------------------------------
;(multirember&co 'tuna (list 'and 'tuna) a-friend)
;  (null? (list 'and 'tuna) #f
;  (eq? 'tuna (car (list 'and 'tuna)))
;  (eq? 'tuna 'and) #f
;  (else (multirember&co 'tuna (cdr (list 'and 'tuna)) (lambda (newlat seen) (a-friend (cons (car (list 'and 'tuna)) newlat) seen))))
;  (else (multirember&co 'tuna (list 'tuna) (lambda (newlat seen) (a-friend (cons 'and newlat) seen))))
;  (else (multirember&co 'tuna (list 'tuna) latest-friend))
;          (null? (list 'tuna)) #f
;          (eq? 'tuna (car (list 'tuna))) #t
;            (multirember&co 'tuna (cdr (list 'tuna)) (lambda (newlat seen) (latest-friend newlat (cons (car (list 'tuna)) seen))))
;            (multirember&co 'tuna '() (lambda (newlat seen) (latest-friend newlat (cons 'tuna seen))))
;            (multirember&co 'tuna '() (lambda (newlat seen) another-friend))
;              (null? '()) #t
;                (another-friend '() '())
;                  (latest-friend '() (cons 'tuna '()))
;                  (latest-friend '() (list 'tuna))
;                    (a-friend (cons 'and '()) (list 'tuna))
;                    (a-friend (list 'and) (list 'tuna))
;                      (null? (list 'tuna)) #f
; #f
; ------------------------------
;(multirember&co 'tuna (list 'strawberries 'tuna 'and 'swordfish) last-friend)
;  (null? (list 'strawberries 'tuna 'and 'swordfish)) #f
;  (eq? 'tuna (car (list 'strawberries 'tuna 'and 'swordfish))
;  (eq? 'tuna 'strawberries) #f
;  (else (multirember&co 'tuna (cdr (list 'strawberries 'tuna 'and 'swordfish))
;                        (lambda (newlat seen) (last-friend (cons (car (list 'strawberries 'tuna 'and 'swordfish)) newlat) seen)))
;  (else (multirember&co 'tuna (list 'tuna 'and 'swordfish)
;                        (lambda (newlat seen) (last-friend (cons 'strawberries newlat) seen)))                ;;;; ANON-1
;    (null? (list 'tuna 'and 'swordfish)) #f
;    (eq? 'tuna (car (list 'tuna 'and 'swordfish))
;    (eq? 'tuna 'tuna) #t
;      (multirember&co 'tuna (cdr (list 'tuna 'and 'swordfish))
;                      (lambda (newlat seen) (ANON-1 newlat (cons (car (list 'tuna 'and 'swordfish) seen))))
;      (multirember&co 'tuna (list 'and 'swordfish)
;                      (lambda (newlat seen) (ANON-1 newlat (cons 'tuna seen))))                               ;;;; ANON-2
;        (null? (list 'and 'swordfish)) #f
;        (eq? 'tuna (car (list 'and 'swordfish))
;        (eq? 'tuna 'and) #f
;        (else (multirember&co 'tuna (cdr (list 'and 'swordfish))
;                              (lambda (newlat seen) (ANON-2 (cons (car (list 'and 'swordfish)) newlat) seen))))
;        (else (multirember&co 'tuna (list 'swordfish)
;                              (lambda (newlat seen) (ANON-2 (cons 'and newlat) seen))))                        ;;;; ANON-3
;          (null? (list 'swordfish)) #f
;          (eq? 'tuna (car (list 'swordfish)))
;          (eq? 'tuna 'swordfish) #f
;          (else (multirember&co 'tuna (cdr (list 'swordfish))
;                                (lambda (newlat seen) (ANON-3 (cons (car (list 'swordfish)) newlat) seen ))))
;          (else (multirember&co 'tuna '()
;                                (lambda (newlat seen) (ANON-3 (cons 'swordfish newlat) seen ))))    ;;;; ANON-4
;            (null? '()) #t
;              (ANON-4 '() '())
;                (ANON-3 (cons 'swordfish '()) '() ))))
;                (ANON-3 (list 'swordfish) '() ))))
;                  (ANON-2 (cons 'and (list 'swordfish)) '()))
;                  (ANON-2 (list 'and 'swordfish) '())
;                    (ANON-1 (list 'and 'swordfish) (cons 'tuna '()))
;                    (ANON-1 (list 'and 'swordfish) (list 'tuna))
;                      (last-friend (cons 'strawberries (list 'and 'swordfish)) (list 'tuna))
;                      (last-friend (list 'strawberries 'and 'swordfish) (list 'tuna))
;                        (length (list 'strawberries 'and 'swordfish))
;                        3
; 3
; ------------------------------

; ------------------------------
(define multiinsertL          ; interactions pane will lowercase to multiinsertl
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multiinsertR          ; interactions pane will lowercase to multiinsertr
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multiinsertLR         ; interactions pane will lowercase to multiinsertlr
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))
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

(define list7
  (list '+ 5 3))

(define list8
  (list 'shrimp 'salad 'tuna 'salad 'and 'tuna))

(define list9
  (list 'strawberries 'tuna 'and 'swordfish))