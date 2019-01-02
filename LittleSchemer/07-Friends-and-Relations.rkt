#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Not a set because apple appears more than once
'(apple peaches apple plum)

; Set, no atom appears more than once
'(apple peaches pears plums)
'()

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))
    
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '(apple peaches apple plum))
(set? '(apple peaches pears plums))

(set? '(apple 3 pear 4 9 apple 3 4))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat)))))
      )))

(makeset2 '(apple peach pear peach plum apple lemon peach))

(makeset2 '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))
      

(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)    
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      ((cons (car set1) (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

(define setdiff
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (setdiff (cdr set1) set2))
      (else (cons (car set1) (setdiff (cdr set1) set2))))))

(setdiff '(1 2 3 4) '(1 3))
      
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set))))
      )))

(intersectall
 '((a b c)
   (c a d e)
   (e f g h a b)))

(intersect '(c a d e) '(e f g h a b))
(intersect '(a b c) '(a e))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (x1 x2)
    (cons x1 (cons x2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel)))
                  (revrel (cdr rel))))
      )))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define revpair
  (lambda (p)
    (build (second p) (first p))
    ))

(define revrel2
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel))))
      )))

(revrel2 '((8 a) (pumpkin pie) (got sick)))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel)) (seconds (cdr rel)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(seconds '((grape raisin) (plum prune) (stewed grape)))

(fullfun? '((grape raisin) (plum prune) (stewed prune)))
(fullfun? '((grape raisin) (plum prune) (stewed grape)))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel2 fun))))

(one-to-one? '((grape raisin) (plum prune) (stewed prune)))
(one-to-one? '((grape raisin) (plum prune) (stewed grape)))



 

       