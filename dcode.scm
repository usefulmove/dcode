;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

#|  (dcode )  |#

; fold :: (T -> T) -> T -> [T] -> T
(define (fold f acc lst)
  (cond [(null? lst) acc]
        [else (fold f (f acc (car lst)) (cdr lst))]))

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

; map :: (T -> U) -> [T] -> [U]
(define (map f lst)
  (cond [(null? lst) '()]
        [else (cons (f (car lst)) (map f (cdr lst)))]))

; filter :: (T -> bool) -> [T] -> [T]
(define (filter f lst)
  (cond [(null? lst) '()]
        [(not (f (car lst))) (filter f (cdr lst))]
        [else (cons (car lst) (filter f (cdr lst)))]))



#|   "I think differently in Scheme."   |#
