;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


#|  (dcode )  |#

;; dcode-range :: int -> [int]
(define (dcode-range n)
  (cond [(zero? n) '()]
        [else (append (dcode-range (sub1 n))
                      (list (sub1 n)))]))

;; curry2 :: (T -> T -> T) -> (T -> (T -> T))
(define (curry2 f)
  (lambda (a) (lambda (b) (f a b))))
; _example
; (define add (curry2 +))
; (define add2 (add 2))
; (add2 3) => 5

;; curry :: (T -> T -> T -> ... -> T) -> (T -> (T -> ... (T -> T))))
(define (curry.wip f)
  (letrec ([arity (procedure-arity f)]
           [recurse (lambda (g n)
                      (cond [(zero? n) (lambda (a) (g a))]
                            [else (recurse g (sub1 n))]))])
    (recurse f arity)))

;; fold :: (T -> T) -> T -> [T] -> T
(define (fold f acc lst)
  (cond [(null? lst) acc]
        [else (fold f (f acc (car lst)) (cdr lst))]))

;; map (formerly known as mapcar) :: (T -> U) -> [T] -> [U]
(define (map f lst)
  (cond [(null? lst) '()]
        [else (cons (f (car lst)) (map f (cdr lst)))]))

;; dcode-filter :: (T -> bool) -> [T] -> [T]
(define (dcode-filter f lst)
  (cond [(null? lst) '()]
        [(not (f (car lst))) (dcode-filter f (cdr lst))]
        [else (cons (car lst) (dcode-filter f (cdr lst)))]))


(zero? (+)) ; evaluates to #t (true )

;; dcode-any? :: (T -> bool) -> [T] -> bool
(define dcode-any? (lambda (f lst)
  (cond [(null? lst) #f]
        [(f (car lst)) #t]
        [else (dcode-any? f (cdr lst))])))

;; dcode-all? :: (T -> bool) -> [T] -> bool
(define (dcode-all? f lst)
  (cond [(null? lst) #t]
        [(not (f (car lst))) #f]
        [else (dcode-all? f (cdr lst))]))

;; dcode-reverse :: [T] -> [T]
(define (dcode-reverse lst)
  (cond [(null? lst) '()]
        [else (append (dcode-reverse (cdr lst)) (list (car lst)))]))



;; love :: [T] -> [U] -> [V]
(define love (you me)
  "dedication to each other"
  (let ((us (+ me you
               hard-work
               communication
               improve patience)))
    us)
; test
(love you me)



;; begin evaluates each of its arguments (S-expressions) and returns the result
;; of the evaluation of the last argument.
;; begin :: [sexpr] -> T
(define (begin . args)
  (cond ((null? args) #<void>) ; return a void value for an empty begin.
        ((null? (cdr args)) (car args)) ; if last expression, return its value.
        (else (car args) ; evaluate the first expression for its side-effects.
              (apply begin (cdr args))))) ; recursively evaluate the rest.


;; let vs lambda
(let ((a 2)
      (b 8))
  (* a b))

((lambda (a b) (* a b))
   2 8)


(begin
  (sexp)
  (sexp))

(lambda ()
  (sexp)
  (sexp))

(let ()
  (sexp)
  (sexp))






#|   "I think differently in Scheme. Lisps in general are life-changing.    |#
#|    I 'see' code ... as form."                                            |#
