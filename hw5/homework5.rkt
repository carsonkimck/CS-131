;; Carson Kim
;; CS 131 - Winter 2021
;; Homework #5
#lang racket

; map everything after the first lambda,,,,, map everything after the second lambda

; two maps,....apply it on the function body..if it's in the map, pull it form the map, if it's not; 
; then just compare it regularly

;;string to symbol function in scheme

;; PROBLEM 1:

(define (lambda? x)
  (member x '(lambda λ)))

(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) 
         (if x '% '(not %))]
        ; if one of them is not list - which means that not function
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]
        [(and(equal? (car x) 'if) (not(equal? (car y) 'if))) (list 'if '% x y)]
        [(and(not(equal? (car x) 'if)) (equal? (car y) 'if)) (list 'if '% x y)]
         [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]
        [(and (list? x) (list? y) (equal? (length x) (length y))) (compare-lists x y)]
        [else (list 'if '% x y)]

        
        ))


(define (compare-lists x y)
  (cond
   [(and (empty? x) (empty? y)) '()]
   [(and (list? (car x)) (list? (car y)))(cons (compare-lists (car x) (car y)) (compare-lists(cdr x) (cdr y)))] 
    [(and (equal? (car x) 'cons) (equal? (car y )'list)) (list 'if '% 'cons 'list (cdr x))]
    [(and (equal? (car x) (car y)) (cons (car x) (compare-lists (cdr x) (cdr y))))]
    [else (cons (list 'if '% (car x) (car y)) (compare-lists (cdr x) (cdr y)))]
     
    ))
                  



(expr-compare '(if x y z) '(g x y z))

;;((if % cons list) a b)

;; PROBLEM 2: 
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

  
;; PROBLEM 3: 
(define test-expr-x 
  `(cons 5 ((lambda (a) (+ a 3)) 4)))

(define test-expr-y
  `(cons 6 ((lambda (a) (+ a 7)) 10)))


