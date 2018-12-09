#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define rember
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? a (car l)) (cdr l))
      (else (cons (car l) (rember a (cdr l)))))))
; I wrote the previous function as an exercise before continuing, so working through
; understanding the need for the cons piece of this function is omitted.
; It's necessary to cons the (car l) onto the result of the recursion so then
; the original list is rebuilt on the way out of the call stack.6

(rember 'mint '(lamb chops and mint jelly))
    
(rember 'bacon '(bacon lettuce and tomato))

(rember 'and '(bacon lettuce and tomato))

(rember 'sauce '(soy sauce and tomato sauce))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts
 '(
   (apple peach pumpkin)
   (plum pear cherry)
   (grape raisin pea)
   (bean carrot eggplant)))

(firsts
 '((a b) (c d) (e f)))

; The Third Commandment
; When building a list, describe teh first typical element, and
; then cons it onto the natural recursion.

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))

(insertR 'jalapeno 'and '(tacos tamales and salsa))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'test 'typing '(welcome to the typing test))
; just a quick test since there isn't one in the book

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'topping 'fudge '(ice cream with fudge for dessert))    




