#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

14

(atom? 14)

-3
; but we do not consider negative numbers

3.14159
; but we consider only whole numbers

(add1 67)
; add1 is already defined in Racket

(sub1 5)

(sub1 0)
; No answer because we only consider positive numbers

(zero? 0)

(zero? 1492)

(define o+
  (lambda (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (add1 (o+ n1 (sub1 n2)))))))

(o+ 46 12)

; zero? is like null?
; add1 is like cons

(define o-
  (lambda (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (sub1 (o- n1 (sub1 n2)))))))

(o- 14 3)

(o- 17 9)

(o- 18 25)
; But no anwer, because we don't consider negative numbers

'(2 11 3 79 47 6)
; this is a tup.  tup is short for tuple

'(8 55 5 555)
; tup, because it's a list of numbers

'(1 2 8 apple 4 3)
; not a tup, just a list of atoms
    
'(3 (7 4) (13 9))
; not a tup, because (7 4) is not a number.  a tup is just a list of numbers

'()
; this is a tup because it's a list of zero numbers.  this is the empty tup

; The First Commandment (first revision)
; When recurring on a list of atoms, lat, ask two questions about
; it: (null? lat) and else.
; When recurring on a number, n, ask two questions about it:
; (zero? n) and else.

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(addtup '(3 5 2 8))

(addtup '(15 6 7 12 3))

(define mult
  (lambda (n1 n2)
    (cond
      ((zero? n2) 0)
      (else (o+ n1 (mult n1 (sub1 n2)))))))
    
(mult 5 3)

(mult 13 4)

; The Fourth Commandment (first revision)
; Always change at least one argument while recurring.  It must be
; changed to be closer to termination.  The changing argument must be
; tested int he termination condition:
; when using cdr, test termination with null? and
; when using sub1, test termination with zero?

(mult 12 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons
             (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))

(tup+ '(2 3) '(4 6))

(tup+ '(3 7) '(4 6))

(tup+ '(3 7) '(4 6 8 1))

(tup+ '(3 7 8 1) '(4 6))

(define gt
  (lambda (n1 n2)
    (cond
      ((eq? n1 0) #f)
      ((eq? n2 0) #t)
      (else (gt (sub1 n1) (sub1 n2))))))

(gt 12 133)
(gt 120 11)
(gt 3 3)

(define lt
  (lambda (n1 n2)
    (cond
      ((eq? n2 0) #f)
      ((eq? n1 0) #t)      
      (else (lt (sub1 n1) (sub1 n2))))))

(lt 4 6)
(lt 8 3)
(lt 6 6)

(define eqx
  (lambda (n1 n2)
    (cond
      ((lt n1 n2) #f)
      ((gt n1 n2) #f)
      (else #t))))

(eqx 5 5)

(define pow
  (lambda (n exp)
    (cond
      ((zero? exp) 1)
      (else (mult n (pow n (sub1 exp)))))))

(pow 1 1)
(pow 2 3)
(pow 5 3)

(define div
  (lambda (num den)
    (cond
      ((< num den) 0)
      (else (add1 (div (o- num den) den))))))

(div 6 2)

(div 15 4)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(length '(hotdogs with mustard sauerkraut and pickles))

(length '(ham and cheese on rye))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(define rempick
  (lambda (n lat)
    (cond      
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))

(number? 'tomato)

(number? 76)

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (s1 s2)
    (cond
      ((and (number? s1) (number? s2)) (eqx s1 s2))
      ((or (number? s1) (number? s2)) #f)
      (else (eq? s1 s2)))))

(eqan? 5 5)
(eqan? 'five 5)
(eqan? 'five 'five)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(occur 5 '(1 2 3 4 5))
(occur 5 '(1 5 3 5 5))
(occur 'five '(1 five 2 five 3 4 5))

(define one?
  (lambda (n)
    (cond
      ((zero? (sub1 n)) #t)
      (else #f))))

(one? 1)
(one? 2)
(one? 3)
(one? 0)

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))

(rempick2 2 '(peanut butter and jelly))