#lang racket

;;Solutions

;;Question 1: the countdown - solved

(define countdown
  (lambda (n)
    (cond
      [(zero? n) '(0)]
      [else
      (cons n (countdown (sub1 n)))])))

;;Question 2: Insert R - solved
      
(define insertR
  (lambda (a b ls)
    (cond
      [(null? ls) '()]
      [else
         (cond 
           [(eqv? (car ls) a) (cons a (cons b (insertR a b (cdr ls))))]
           (else (cons (car ls) (insertR a b (cdr ls))))
           )])))
            
;;Question 3: Solved

(define remv-1st
  (lambda (a ls)
    (cond
      [(null? ls) '()]
      [else
         (cond 
           [(eqv? (car ls) a) (cdr ls)]
           [else (cons (car ls) (remv-1st a (cdr ls)))]
           )])))


;;Q4-solved

(define count-?s
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else
       (cond
         [(eqv? (car ls) '?) (add1 (count-?s (cdr ls)))]
         [else (count-?s (cdr ls))])])))

;;Q5

(define filter
  (lambda (a ls)
    (cond
      [(null? ls) '()]
      [else
         (cond 
           [(a (car ls)) (cons (car ls) (filter a (cdr ls)))]
           [else (filter a (cdr ls))]
           )])))

;;Q6 - solved

(define zip
  (lambda (ls1 ls2)
    (cond
      [(or (null? ls1) (null? ls2)) '()]
      [else
       (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))  
       ])))

;;Q7 - solved
;;(map add1 '(1 2 3 4))

(define map
  (lambda (a ls)
    (cond
      [(null? ls) '()]
      [else
       (cons (a (car ls)) (map a (cdr ls)))])))

;;Q8 - solved
;;(append '(a b c) '(1 2 3))

(define append
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else
       (cons (car ls1) (append (cdr ls1) ls2))
       ])))

;;Q9 - reverse- not solved
;;(reverse '(a 3 x))
(define (reverse ls)
    (cond
      [(null? (cdr ls)) ls]
        (else(append (reverse (cdr ls)) (list (car ls))))))
;;Q10 - solved
(define fact
  (lambda (a)
    (cond
      [(zero? a) 1]
      [else (* a (fact(sub1 a)))])))

;;Q11 - not solved
(define member-?*
  (lambda (ls)
    (cond
      [(null? ls) #f]
      [else
       (cond
         [(eqv? (car ls) '?) #t]
         [else (member-?* (cdr ls))])])))

;;Q12 Fibonacci - solved

(define fib
  (lambda (n)
    (cond
      [(zero? n) 0]
      [(zero? (sub1 n)) 1]
      [else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))])))

;;Q13 - unsolved but remeber to attempt
;; ((w x) y (z))

;;Q14 - binary->natural
(define binary->natural
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else (+ (binary->natural (cdr ls)) (expt 2 (* (add1 (binary->natural (cdr ls))) (car ls))))]))) 




;;Q15 - minus - solved

(define minus2
  (lambda (x y)
    (cond ((zero? x) y)
        (else (minus2 (sub1 x) (sub1 y))))))
(define minus1
  (lambda (x y)
    (cond ((zero? y) x)
        (else (minus1 (sub1 x) (sub1 y))))))

(define minus
  (lambda (x y)
    (cond ( (and (and (positive? x)(positive? y))(> x y)) (minus1 x y))
          (else
           (cond ((and (positive? x)(positive? y)) (minus2 x y))
                 (else (display 'error)))))))

; Q16

(define div
  (lambda (x y)
    (cond
      [(< x y) 0]
      [else (add1 (div (minus x y) y))])))

;Q17

#;(define append-map
  (lambda (P1 ls))
    (cond
      [(null? ls) '()]
      [else (append ls (append-map (map P1 (cdr ls))))]))

;Q18

#;(define set-difference
  (lambda (L1 L2)
    (cond
      [(null? L2) '()]
      [else (cond
              [(equal? (car L1)(car L2)) (set-difference (remv-1st (car L1) L1) L2)]
              [else (set-difference (remv-1st (car L1) L1) L2)])])))