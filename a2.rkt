#lang racket

;; Q1) solved Reference source: http://stackoverflow.com/questions/28245935/racket-list-ref-implementation-half-working

(define list-ref
  (lambda (ls n)
   (letrec
       ((nth-cdr
        (lambda (n)
          (if (number? n)
                (nth-cdr (cons ls n))
                (let ((ls (car n))
                      (n (cdr n)))
                  (if (or (and(number? n)(zero? n)) (null? n))
                      ls
                      (nth-cdr (cons (cdr ls) (sub1 n)))))))))
      (car (if (pair?(nth-cdr n)) (nth-cdr n) (cons(nth-cdr n)'()))))))

;;Q2 solved

(define (union ls1 ls2)
      (if (null? ls2) ls1
          (if (member (car ls2) ls1)
              (union ls1 (cdr ls2))
              (union (append ls1 (list (car ls2))) (cdr ls2)))))

;;Q3 solved
 
(define extend
         (lambda (x pred)
           (lambda (n)
                  (or (eqv? x n) (pred n)))))

;;Q4 solved

(define walk-symbol
  (lambda (x s)
    (cond
      [(null? s) x]
      [else (cond
              [(and (pair? (assv x s)) (symbol? (cdr (assv x s)))) (walk-symbol (cdr (assv x s)) (remove (assv x s) s))]
              [else (cond
                      [(eqv? #f (and (pair? (assv x s)) (cdr (assv x s)))) x]
                      [else (and (pair? (assv x s)) (cdr (assv x s)))])])])))

;;Q5 - Solved

(define lambda->lumbda
  (lambda (exp)
    (match exp
      ;[`,x #:when (and (symbol? x) (eqv? x 'lambda)) 'lumbda]
      [`,x #:when (symbol? x) `,x]
      [`(,x) #:when (symbol? x) `(,x)]
      
      ;[`(lambda (,y) ,body) #:when (and (symbol? y) (eqv? y 'lambda)) `(lumbda (lambda) ,(lambda->lumbda body))]
      [`(lambda (,y) ,body) `(lumbda (,y) ,(lambda->lumbda body))]
      
      [`(,rator ,rand) `(,(lambda->lumbda rator)
                               ,(lambda->lumbda rand))]
      )))

;;Q6 - solved

(define var-occurs?
  (lambda (s exp)
    (match exp
      [`,x #:when (and (symbol? x) (eqv? s x)) #t]
      [`,x #:when (symbol? x) #f]
      [`(lambda (,y) ,body) (var-occurs? s body)]
      [`(,rator ,rand) (or (var-occurs? s rator)
                               (var-occurs? s rand))])))

;;Q7 - Solved

(define vars
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) `(,x)]
      [`(lambda (,y) ,body) (vars body)]
      [`(,rator ,rand) (append (vars rator)
                               (vars rand))])))

;;Q8 - solved

(define unique-vars
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) `(,x)]
      [`(lambda (,y) ,body) (unique-vars body)]
      [`(,rator ,rand) (union (unique-vars rator)
                               (unique-vars rand))])))

;;Q9 - solved

(define var-occurs-free?
  (lambda (s exp)
    (match exp
      [`,x #:when (and (symbol? x) (eqv? s x)) #t]
      [`,x #:when (symbol? x) #f]
      [`(,x) #:when (symbol? x) #f]
      [`(lambda (,y) ,body) #:when (eqv? s y) (var-occurs-free? s (remove y body))]
      [`(lambda (,y) ,body) (var-occurs-free? s body)]
      [`(,rator ,rand) (or (var-occurs-free? s rator)
                               (var-occurs-free? s rand))])))

;;Q10 - solved partially

(define member?
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [else (cond
              [(eqv? x (car ls)) #t]
              [else (member? x (cdr ls))])])))
  

(define var-occurs-bound?
  (lambda (s exp)
    (match exp
      [`,x #:when (symbol? x) #f]
      [`(lambda (,y) ,body) #:when (and (eqv? s y) (list? body)) (cond ((member? s (list-flatten body)) #t) (else #f))]
      [`(lambda (,y) ,body) #:when (eqv? s y) (cond ((member? s (list-flatten body)) #t) (else #f))]
      [`(lambda (,y) ,body) (var-occurs-bound? s body)]
      [`(,rator ,rand) (or (var-occurs-bound? s rator)
                               (var-occurs-bound? s rand))])))

;;Q11 - solved

(define list-flatten
  (lambda (x)
    (cond
      [(null? x) '()]
      [(not (pair? x)) (list x)]
      [else (append (list-flatten (car x)) (list-flatten (cdr x)))])))

(define (del V L)
  (cond ((null? L) L)
        ((list? (car L))
         (cons (del V (car L)) (del V (cdr L))))
        ((equal? V (car L)) (del V (cdr L)))
        (else (cons (car L) (del V (cdr L))))))


(define unique-free-vars
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) `(,x)]
      [`(,x) #:when (symbol? x) `(,x)]
      [`(,ls) #:when (list? ls) (unique-free-vars ls)]

      [`(lambda (,y) ,body) #:when (or (and (pair? body) (member? y (list-flatten body))) (and (eqv? y body) (symbol? body))) (unique-free-vars(del y body))]      
      [`(lambda (,y) ,body) #:when (and (pair? body) (member? 'lambda body)) (unique-free-vars body)]
      [`(lambda (,y) ,body) (unique-free-vars (remove y body))]
      
      [`(,rator ,rand) (union (unique-free-vars rator)
                               (unique-free-vars rand))])))

;;Q12

(define unique-bound-vars
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) `()]

      [`(lambda (,y) ,body) #:when (and (pair? body) (member? 'lambda body)) (unique-bound-vars body)]
      [`(lambda (,y) ,body) #:when (or (and (pair? body) (member? y (list-flatten body))) (and (eqv? y body) (symbol? body))) `(,y)]      
      [`(lambda (,y) ,body) (unique-bound-vars body)]
      
      [`(,rator ,rand) (union (unique-bound-vars rator)
                               (unique-bound-vars rand))])))

;;Q13 - not solved


#;(define lex
  (lambda (exp env)
    (match exp
      [(? symbol?) (list-ref exp env)]

      [`(lambda (,x) ,b) (let ([x (gensym)])`(lambda (,x),(lex b (cons x env))))]
      [`(,rator ,rand) `(,(lex rator env) ,(lex rand env))])))

;;Q14 -

#;(define walk-symbol-update
  (lambda (x s)
    (cond
      [(null? s) x]
      [else (cond
              [(and (pair? (assv x s)) (symbol? (cdr (assv x s)))) (walk-symbol (cdr (assv x s)) (remove (assv x s) s))]
              [else (cond
                      [(eqv? #f (and (pair? (assv x s)) (cdr (assv x s)))) x]
                      [else (and (pair? (assv x s)) (cdr (assv x s)))])])])))