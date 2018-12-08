#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'atom)

(atom? 'turkey)

(atom? '1492)

(atom? 'u)

(atom? '*abc$)

(atom? '(atom))

'(atom turkey or)

'(atom turkey) 'or

'((atom turkey) or)

'xyz

'(x y z)

'((x y) z)

'(how are you doing so far)

'(((how) are) ((you) (doing so) far))

'()

'(() () ())

(car '(a b c))

(car '((a b c) x y z))

; (car 'hotdog)
; no answer, can't car an atom

; (car '())
; no answer, can't car an empty list

; The Law of car
; The primitive car is defined only for non-empty lists.

(car '(((hotdogs)) (and) (pickle) relish))

(car (car '(((hotdogs)) (and))))

(cdr '(a b c))

(cdr '((a b c) x y z))

(cdr '(hamburger))

(cdr '((x) t r))

; (cdr 'hotdogs)
; no answer, can't cdr an atom

; (cdr '())
; no answer, can't cdr an empty list

; The Law of Cdr
; The primitive cdr is defined only for non-empty lists.  The cdr of any non-empty list is always another list.

(car (cdr '((b) (x y) (c))))

(cdr (cdr '((b) (x y) (c))))

;(cdr (car '(a (b (c)) d)))
; no answer, a is an atom so can't use cdr

(cons 'peanut '(butter and jelly))

(cons '(banana and) '(peanut butter and jelly))

(cons '((help) this) '(is very ((hard) to learn)))

; cons takes two arguments: the first one is any S-expression;  the second one is any list.

(cons '(a b (c)) '())

(cons 'a '())

; the following two should be "no answer" because the second S-expression is not a list.

(cons '((a b c)) 'b)
; not sure why this worked or what the output means.  Output is: (((a b c)) . b)

(cons 'a 'b)
; not sure why this worked

; The Law of Cons
; The primitive cons take two arguments.  The second argument to cons must be a list.  The result is a list.

(cons 'a (car '((b) c d)))

(cons 'a (cdr '((b) c d)))

(null? '())

(null? '(a b c))

(null? 'spaghetti)
; According to Law of Null this shouldn't work but it generally works for all values

; The Law of Null?
; The primitive null? is defined only for lists.

(atom? 'Harry)

(atom? '(Harry had a heap of apples))

(atom? (car '(Harry had a heap of apples)))

(atom? (cdr '(Harry had a heap of apples)))

(atom? (cdr '(Harry)))

(atom? (car (cdr '(swing low sweet cherry oat))))

(atom? (car (cdr '(swing (low sweet) cherry oat))))

(eq? 'Harry 'Harry)

(eq? 'margarine 'butter)

; eq? takes two arguments.  Both of them must be non-numeric atoms

(eq? '() '(strawberry))
; Should be no answer, but this version of eq? must accept lists.

(eq? 6 7)
; Should be no answer, but this version of eq? accepts numbers.

; The Law of Eq?
; The primitive eq? takes two arguments.  Each must be a non-numeric atom.

(eq? (car '(Mary had a little lamb chop)) 'Mary)

(eq? (cdr '(soured milk)) 'milk)
; #f but should be No Answer since cdr returns a list and eq? primitive does not accept a list.

(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))

; Now go make yourself a peanut butter and jelly sandwich.