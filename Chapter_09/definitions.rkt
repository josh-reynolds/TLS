#lang r5rs
; ------------------------------
; The Little Schemer (Fourth Edition)
; Chapter 9: ... and Again, and Again, and Again,...
; ------------------------------

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
; ------------------------------

; ------------------------------
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))
; ------------------------------

; ------------------------------
(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
; ------------------------------

; ------------------------------
(define sub1
  (lambda (n)
    (- n 1)))
; ------------------------------

;; working out the example -----
;(looking 'caviar list1)
;  (keep-looking 'caviar (pick 1 list1) list1)
;  (keep-looking 'caviar 6 list1)
;    ((number? 6) #t
;      (keep-looking 'caviar (pick 6 list1) list1)
;      (keep-looking 'caviar 7 list1)
;        ((number? 7) #t
;          (keep-looking 'caviar (pick 7 list1) list1)
;          (keep-looking 'caviar 3 list1)
;            ((number? 3) #t
;              (keep-looking 'caviar (pick 3 list1) list1)
;              (keep-looking 'caviar 4 list1)
;                ((number? 4) #t
;                  (keep-looking 'caviar (pick 4 list1) list1)
;                  (keep-looking 'caviar 'caviar list1)
;                    ((number? 'caviar) #f
;                    (else (eq? 'caviar 'caviar))
;                           #t
; #t
; ------------------------------

; ------------------------------
(define eternity              ; infinite recursion !!!
  (lambda (x)
    (eternity x)))
; ------------------------------

; ------------------------------
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
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
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))
; ------------------------------

; ------------------------------
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (length* (first pora))
                (length* (second pora)))))))
; ------------------------------

; ------------------------------
(define add1
  (lambda (n)
    (+ n 1)))
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
(define weight*
  (lambda (pora)
    (cond
    ((atom? pora) 1)
    (else (o+ (times (weight* (first pora)) 2)
              (weight* (second pora)))))))          
; ------------------------------

; ------------------------------
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))
; ------------------------------

; ------------------------------
(define revpair
  (lambda (p)
    (build (second p)
           (first p))))
; ------------------------------

;; working out the example -----
; (shuffle list4)
;   (atom? list4) #f
;   (a-pair? (first list4))
;   (a-pair? (list 'a 'b))
;     (atom? (list 'a 'b)) #f
;     (null? (list 'a 'b)) #f
;     (null? (cdr (list 'a 'b)))
;     (null? (list 'b)) #f
;     (null? (cdr (cdr (list 'a 'b))))
;     (null? '()) #t
;   (a-pair? (list 'a 'b)) #t
;     (shuffle (revpair list4))
;     (shuffle (build (second list4) (first list4)))
;     (shuffle (build (list 'c 'd) (list 'a 'b)))
;     (shuffle (cons (list 'c 'd) (cons (list 'a 'b) '())))
;     (shuffle (list (list 'c 'd) (list 'a 'b)))   # LIST-A
;       (atom? LIST-A) #f
;       (a-pair? (first LIST-A)
;       (a-pair? (list 'c 'd))
;         (atom? (list 'c 'd)) #f
;         (null? (list 'c 'd)) #f
;         (null? (cdr (list 'c 'd)))
;         (null? (list 'd)) #f
;         (null? (cdr (cdr (list 'c 'd))))
;         (null? '()) #t
;       (a-pair? (list 'c 'd))#t
;         (shuffle (revpair LIST-A))
;         (shuffle (build (second LIST-A) (first LIST-A)))
;         (shuffle (build (list 'a 'b) (list 'c 'd)))
;         (shuffle (cons (list 'a 'b) (cons (list 'c 'd) '())))
;         (shuffle (list (list 'a 'b) (list 'c 'd)))   # list4
; infinite recursion !!!
; ------------------------------

; ------------------------------
(define C                     ; interactions pane will lowercase to c
  (lambda (n)                 ; Lothar Collatz (https://en.wikipedia.org/wiki/Lothar_Collatz)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (divide n 2)))
         (else (C (add1 (times 3 n)))))))))
; ------------------------------

; ------------------------------
(define one?
  (lambda (n)
    (= n 1)))
; ------------------------------

; ------------------------------
(define divide                ; text uses typographic division symbol
  (lambda (n m)               ; 'quotient' is the defined procedure for this same function
    (cond
      ((lt? n m) 0)
      (else (add1 (divide (o- n m) m))))))
; ------------------------------

; ------------------------------
(define lt?                   ; text uses '<' but this is already a defined primitive operation
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt? (sub1 n) (sub1 m))))))
; ------------------------------

; ------------------------------
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))
; ------------------------------

; ------------------------------
(define A                     ; interactions pane will lowercase to a
  (lambda (n m)               ; Wilhelm Ackermann (https://en.wikipedia.org/wiki/Wilhelm_Ackermann)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))
; ------------------------------

; ------------------------------
;(define will-stop?
;  (lambda (f)
;    ...))
; ------------------------------

; ------------------------------
;(define last-try
;  (lambda (x)
;    (and (will-stop? last-try)
;         (eternity x))))
; ------------------------------

;; working out the example -----
; (last-try '())
; (and (will-stop? last-try)
;      (eternity '()))
; EITHER
; (and #f #f) #f
; OR
; (and #t #f) #f
;
; Alan Turing (https://en.wikipedia.org/wiki/Alan_Turing)
; Kurt Gödel (https://en.wikipedia.org/wiki/Kurt_Gödel)
; ------------------------------

; ------------------------------
(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
; ------------------------------

; ------------------------------
(lambda (l)                   ; length-sub-zero
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))
; ------------------------------

; ------------------------------
(lambda (l)                   ; length-sub-less-than-one
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 (eternity (cdr l)))))) (cdr l))))))
; ------------------------------

; ------------------------------
((lambda (length)             ; length-sub-zero
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)
; ------------------------------

; ------------------------------
((lambda (f)                   ; length-sub-less-than-one
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))
; ------------------------------

; ------------------------------
((lambda (length)             ; length-sub-less-than-two
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)))
; ------------------------------

; ------------------------------
((lambda (mk-length)          ; length-sub-zero
  (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
; ------------------------------

; ------------------------------
((lambda (mk-length)          ; length-sub-less-than-one
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
; ------------------------------

; ------------------------------
((lambda (mk-length)          ; length-sub-less-than-two
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
; ------------------------------

; ------------------------------
((lambda (mk-length)          ; length-sub-less-than-three
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
; ------------------------------

; ------------------------------
((lambda (mk-length)          ; length-sub-zero
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
; ------------------------------

; ------------------------------
((lambda (mk-length)          ; length-sub-zero
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))
; ------------------------------

(define list1
  (list 6 2 4 'caviar 5 7 3))

(define list2
  (list 6 2 'grits 'caviar 5 7 3))

(define list3
  (list (list 'a 'b) 'c))

(define list4
  (list (list 'a 'b)
        (list 'c 'd)))

(define list5
  (list 'a
        (list 'b 'c)))


