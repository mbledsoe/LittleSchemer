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




















