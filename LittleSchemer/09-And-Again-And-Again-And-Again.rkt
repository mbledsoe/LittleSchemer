#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define pick
 (lambda (n lat)
   (cond
     ((eq? n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 3 '(one two three))
(pick 2 '(a b c))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (x1 x2)
    (cons x1 (cons x2 '()))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(shift '((a b) c))
(shift '((a b) (c d)))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora))))
     )))

(align '((a b)(c d)))
(align '(((a b) (c d)) ((e f) (g h))))

(define length*
  (lambda (pora)
    (cond      
      ((atom? pora) 1)      
      (else (+ (length* (first pora))
               (length* (second pora))))
      )))

; note: length* only works for pairs, not arbitrary lists.
(length* '((a b) c))
(length* '((a b) (c d)))
(length* '(((a b) (c d)) ((e f) (g h))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2)
               (weight* (second pora))))    
      )))

(weight* '((a b) c))
(weight* '(a (b c)))

(define revpair
  (lambda (p)
    (build (second p) (first p))
    ))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora))))
      )))

(shuffle '(a (b c)))
(shuffle '((a b) c))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m))))
      )))

(A 1 1)
(A 2 2)

; Explanation of why will-stop can't be defined...

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

(length '(1 2 3))

(define eternity
  (lambda (x)
    (eternity x)))


; length0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

; length <= 1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
           ((lambda (l)
             (cond
               ((null? l) 0)
               (else (add1 (eternity (cdr l)))))) (cdr l))))
    ))

; length <= 2
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1               
               (lambda (l)
                 (cond
                   ((null? l) 0)
                   (else (add1
                          ((lambda (l)
                             (cond
                               ((null? l) 0)
                               (else (add1 (eternity (cdr l)))))) (cdr l))))
                   ))))))

; length0
; immediate invoked style
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))
       )))
 eternity)

; length<=1
; immediate invoked style
((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l))))
       )))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
 eternity))

; length<=2
; immediate invoked style
(((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))
       )))
 ((lambda (f)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (f (cdr l))))
        )))
  ((lambda (g)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (g (cdr l)))))))
   eternity))) '(1 2))

; length <= 1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; length <= 2
(lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; the recursive tower runs out when we hit the eternity function
; using mk-length to get one more function before we hit eternity
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity) (cdr l))))))))

; Does it work?
; Only for <= 1
(
 ((lambda (mk-length)
   (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l))))))))
 '(1))

; To create infinity function calls, we need to pass mk-length to mk-length instead of eternity.
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length) (cdr l))))))))

; Re-introducing length function
; Note - this does not work because mk-length is immediately applied and causes infinite evaluation
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))




               