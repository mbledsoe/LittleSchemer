#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqan?
  (lambda (s1 s2)
    (cond
      ((and (number? s1) (number? s2)) (eq? s1 s2))
      ((or (number? s1) (number? s2)) #f)
      (else (eq? s1 s2)))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2) #f))
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(define rember-f
  (lambda (test? a lat)
    (cond
      ((null? lat) '())
      ((test? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember-f test? a (cdr lat)))))))

(rember-f = '5 '(6 2 5 3))
(rember-f = '5 '(6 2 5 3 5))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))

(eq?-salad 'salad)
(eq?-salad 'not-salad)
(eq?-salad '(salad))
(eq?-salad 'tuna)

((eq?-c 'salad) 'tuna)

(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f2 test?) a (cdr l))))))))

(define rember-eq?
  (rember-f2 eq?))

(rember-eq? 'tuna '(tuna salad is good))

((rember-f2 eq?) 'tuna '(tuna salad is good))

((rember-f2 eq?) 'tuna '(shrimp salad and tuna salad))

((rember-f2 eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))


(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (cons new (cons old (cdr lat))))
        (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

(define insert-g
  (lambda (seq) 
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((eq? (car lat) old) (seq new old (cdr lat)))
        (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))


(define seqL
  (lambda (new old l)
    (cons new (cons old (l)))))

(define seqR
  (lambda (new old l)
    (cons old (cons new (l)))))

