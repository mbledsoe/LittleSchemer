#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'x) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      (else #f)
      )))

; all true
(numbered? '(3 + 2))
(numbered? '(3 x 2))
(numbered? '(3 ^ 2))

; all false
(numbered? '(3 / 2))
(numbered? '(mike * 2))

(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp))))))
      )))

; all true
(numbered2? '(3 + 2))
(numbered2? '(3 x 2))
(numbered2? '(3 ^ 2))

; this is true since we don't check for the specific operator
(numbered2? '(3 / 2))

; this is false
(numbered2? '(mike * 2))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x)
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((expt (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      )))

(value 13)
(value '(3 x 5))
(value '(13 + 4))
(value '(1 + (3 ^ 4)))

; The Seventh Commandment
; Recur on the subparts that are of the same nature:
; - On the sublists of a list
; - On the subexpressions of an arithmetic expression.

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value2 (1st-sub-exp nexp))
          (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x)
       (* (value2 (1st-sub-exp nexp))
          (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '^)
       (expt (value2 (1st-sub-exp nexp))
          (value2 (2nd-sub-exp nexp))))
      )))      

(value2 13)
(value2 '(+ 13 4))
(value '(1 + (3 ^ 4)))

; We could change the representation used by original value by using the help functions and changing
; their implementation

; The Eight Commandment
; Use help functions to abstract from representations.

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define plus
  (lambda (n1 n2)
    (cond
      ((null? n1) '())
      (cons ('() (plus (cdr n1) (n2)))))))

(define plus2
  (lambda (n1 n2)
    (cond
      ((sero? n2) n1)
      (else (edd1 (plus2 n1 (zub1 (n2))))))))

; You must beware of shadows.

      



    