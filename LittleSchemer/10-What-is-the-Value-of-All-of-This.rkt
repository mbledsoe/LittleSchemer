#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define build
  (lambda (x1 x2)
    (cons x1 (cons x2 '()))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define new-entry build)

(new-entry
 '(appetizer entree beverage)
 '(pate boeuf vin))

(new-entry
 '(appetizer entree beverage)
 '(beer beer beer))

(new-entry
 '(beverage dessert)
 '((food is) (number one with us)))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f))
      )))

(lookup-in-entry 'entree
                 (new-entry
                  '(appetizer entree beverage)
                  '(pate boeuf vin))
                 (lambda (name) (display "Could not find name")))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      ((lookup-in-entry-help
        name
        (car table)
        (lambda (name)
          (lookup-in-table name (cdr table) table-f))))
      )))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e))
      )))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) * const)
      (else *identifier)
      )))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? e)
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)
         ))
      (else *application)
      )))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e))
      )))
                
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define initial-table
  (lambda (name)
    (car '())))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines) table)))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (question)
    (cond
      ((atom? question) (eq? 'else question))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlist
  (lambda (arglist table)
    (cond
      ((null? arglist) '())
      (else (cons (meaning (car arglist)) (evlist (cdr arglist) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (meaning (arguments of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (e) ((eq? 'primitive (first e)))))

(define non-primitive?
  (lambda (e) ((eq? 'non-primitive (first e)))))

(define apply
  (lambda (f a)
    (cond
      ((primitive? f) (apply-primitive (second f) a))
      ((non-primitive? f) (apply-closure (second f) a)))))


  



  



     