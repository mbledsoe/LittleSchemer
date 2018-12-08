#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; (lat? '(Jack sprat could eat no chicken fat))
; True, because each S-expression in l is an atom

; (lat? '((Jack) Sprat could eat no chicken fat))
; False, because (car l) is a list

; (lat? '(Jack (Sprat could) eat no chicken fat))
; False, because one of the S-expressions is a list

; (lat? '())
; True, because it does not contain a list

; a lat is a list of atoms

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? '(bacon and eggs))

; After this, the book contains a bunch of explanation of how to follow
; the recursive execution of this function.

(or (null? '()) (atom? '(d e f g)))

(or (null? '(a b c)) (null? '()))

(or (null? '(a b c)) (null? '(atom)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? a (car lat)) (member? a (cdr lat)))))))

(member? 'tea '(coffee tea or milk))

(member? 'poached '(fried eggs and scrambled eggs))

(member? 'meat '(mashed potatoes and meat gravy))

; The First Commandment (preliminary)
; Always ask null? as the first question in expressing any function.

(member? 'liver '(bagels and lox))

; Most of this chapter is walking through recursive function
; step by step so that the reader can see how each function
; executes.  There isn't much to type up that isn't just tracing
; through the execution of the recursive function, so I'm going
; to skip that.














