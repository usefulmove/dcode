;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

#|  (dcode )  |#

; curry2 :: (T -> T -> T) -> (T -> (T -> T))
(define (curry2 f)
  (lambda (a) (lambda (b) (f a b))))
; _example
; (define add (curry2 +))
; (define add2 (add 2))
; (add2 3) => 5

; curry :: (T -> T -> T -> ... -> T) -> (T -> (T -> ... (T -> T))))
(define (curry.wip f)
  (letrec ([arity (procedure-arity f)]
           [recurse (lambda (g n)
                      (cond [(zero? n) (lambda (a) (g a))]
                            [else (recurse g (sub1 n))]))])
    (recurse f arity)))

; fold :: (T -> T) -> T -> [T] -> T
(define (fold f acc lst)
  (cond [(null? lst) acc]
        [else (fold f (f acc (car lst)) (cdr lst))]))

; map :: (T -> U) -> [T] -> [U]
(define (map f lst)
  (cond [(null? lst) '()]
        [else (cons (f (car lst)) (map f (cdr lst)))]))

; filter :: (T -> bool) -> [T] -> [T]
(define (filter f lst)
  (cond [(null? lst) '()]
        [(not (f (car lst))) (filter f (cdr lst))]
        [else (cons (car lst) (filter f (cdr lst)))]))


(zero? (+)) ; evaluates to #t (true )

; any? :: (T -> bool) -> [T] -> bool
(define any? (lambda (f lst)
  (cond [(null? lst) #f]
        [(f (car lst)) #t]
        [else (any? f (cdr lst))])))

; all? :: (T -> bool) -> [T] -> bool
;(define (all? f lst)
;  (cond [(null? lst) #t]
;        [(not (f (car lst))) #f]
;        [else (all? f (cdr lst))]))

; reverse :: [T] -> [T]
(define (reverse lst)
  (cond [(null? lst) '()]
        [else (append (cdr lst) (car lst))]))



#|   "I think differently in Scheme."   |#
