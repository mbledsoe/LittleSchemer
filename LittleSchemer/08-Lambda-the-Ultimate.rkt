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
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insertL (insert-g seqL))

(define insertR (insert-g seqR))

(define insertL2
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons new (cdr l)))
      (else (cons (car l) (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst2
  (insert-g seqS))

; I question the instructional value of this part.  The interface to seqrem
; doesn't seem to make a lot of sense and we have dummy values such
; as #f being passed as a result.  There is no "new" for seqrem since
; it does not perform a substitution or insert.
(define seqrem
  (lambda (new old l) l))

(define rember2
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(rember2 'sausage '(pizza with sausage and bacon))

; The Ninth Commandment
; Abstract common patterns with a new function

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x 'x) *)
      ((eq? x '^) expt)
      )))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(atom-to-function (operator '(+ 5 3)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp))))
      )))

(value (1st-sub-exp '(+ 5 3)))
(value (2nd-sub-exp '(+ 5 3)))
(value '(+ 5 3))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))
        ))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))


(define multiremberT
  (lambda (test? lat)
      (cond
        ((null? lat) '())
        ((test? (car lat)) (multiremberT test? (cdr lat)))
        (else (cons (car lat) (multiremberT test? (cdr lat))))
        )))

(multiremberT eq?-tuna '(shrimp salad tuna salad and))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

(multirember&co 'tuna '() a-friend)

(multirember&co 'tuna '(tuna) a-friend)

(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)

; The Tenth Commandment
; Build functions to collect more than one value at a time.

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))
      )))

(multiinsertLR 'tuna 'fish 'cake '(fish cake))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)       
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
       (cons (car lat) (multiinsertLR&co new
                                      oldL
                                      oldR
                                      (cdr lat)
                                      (lambda (newlat L R) (col (cons (car lat) newlat) L R)))))
      )))

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
                  (lambda (newlat L R)
                    (display newlat)
                    (display "\n")
                    (display L)
                    (display "\n")
                    (display R)
                    (display "\n")))

(define even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))

(even? 2)
(even? 4)
(even? 5)

(define evens-only*
  (lambda (l)
    (cond      
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l))))
      )))

(evens-only* '((2 4 6) 1 (1 2 3) 2 3 4 5 6 7 8))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond      
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newlat evenProd oddSum)
                            (col (cons (car l) newlat)
                                 (* (car l) evenProd)
                                 oddSum))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newlat evenProd oddSum)
                            (col newlat
                                 evenProd
                                 (+ (car l) oddSum)))))))
      (else (evens-only*&co (car l)
                            (lambda (al ap as)
                              (evens-only*&co (cdr l)
                                              (lambda (dl dp ds)
                                                (col (cons al dl)
                                                     (* ap dp)
                                                     (* as ds)))))))
      )))


(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
                
