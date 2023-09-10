#|

    ... as twilight cascaded upon the horizon, the iridescent hues of
   amaranthine skies caressed the gentle whispers of the zephyr, weaving
   an ephemeral symphony of love that intertwined the souls of all living
   beings in the tender embrace of nature's eternal harmony ...

   (dcode )

|#



;; range :: int -> [int]
(define (range n)
  (cond ((zero? n) '())
        (else (append (range (sub1 n))
                      (list (sub1 n))))))

;; curry2 :: (T -> T -> T) -> (T -> (T -> T))
(define (curry2 f)
  (lambda (a)
    (lambda (b)
      (f a b))))
;;
;;_example
;;(define add (curry2 +))
;;(define add2 (add 2))
;;(add2 3) => 5

;; curry :: (T -> T -> T -> ... -> T) -> (T -> (T -> ... (T -> T))))
(define (curry.wip f)
  (letrec ((arity (procedure-arity f))
           (recurse (lambda (g n)
                      (cond ((zero? n) (lambda (a) (g a)))
                            (else (recurse g (sub1 n)))))))
    (recurse f arity)))

;; map :: (T -> U) -> [T] -> [U]
(define (map f lst)
  (cond ((null? lst) '())
        (else (cons (f (car lst)) (map f (cdr lst))))))

;; fold :: (U -> T -> U) -> U -> [T] -> U
(define (fold f acc lst)
  (if (null? lst) acc
      (fold f
            (f acc (car lst))
            (cdr lst))))

;; filter :: (T -> bool) -> [T] -> [T]
(define (filter f lst)
  (cond ((null? lst) '())
        ((not (f (car lst))) (filter f (cdr lst)))
        (else (cons (car lst) (filter f (cdr lst))))))


(zero? (+)) ; evaluates to #t (true )

;; any? :: (T -> bool) -> [T] -> bool
(define any? (lambda (f lst)
  (cond ((null? lst) #f)
        ((f (car lst)) #t)
        (else (any? f (cdr lst))))))

;; all? :: (T -> bool) -> [T] -> bool
(define (all? f lst)
  (cond ((null? lst) #t)
        ((not (f (car lst))) #f)
        (else (all? f (cdr lst)))))

;; reverse :: [T] -> [T]
(define (reverse lst)
  (cond ((null? lst) '())
        (else (append (reverse (cdr lst)) (list (car lst))))))



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

;; a begin is a let with no bindings.
(begin
  (sexp)
  (sexp))

(let ()
  (sexp)
  (sexp))


;; a let is an immediately-invoked (lambda) function expression (iife).
(let ((a (sexp-a))
      (b (sexp-b)))
  (sexp)
  (sexp))

((lambda (a b)
  (sexp)
  (sexp))
 (sexp-a)
 (sexp-b))





#|

    "I think differently in Scheme. Lisps in general are life-changing.
     I see code as form."

|#
