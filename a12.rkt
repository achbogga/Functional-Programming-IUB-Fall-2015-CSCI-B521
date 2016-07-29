;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;High Level discussion with Debasis Dwivedy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "C311/monads.rkt")

(define assv-maybe
 (lambda (var lis)
  (cond
   [(eqv? lis '()) (fail)]
   [(eqv? (car (car lis)) var) (return-maybe (cdr (car lis)))]
   [else (assv-maybe var (cdr lis))])))


(define partition-writer
 (lambda (p lis)
  (cond
   [(eqv? lis '()) (return-writer '())]
   [(p (car lis)) (bind-writer (tell-writer (car lis)) (lambda (x) (partition-writer p (cdr lis))))]
   [else (bind-writer (partition-writer p (cdr lis)) (lambda (d) (return-writer (cons (car lis) d))))])))


(define power
 (lambda (var n)
  (cond
   [(zero? n) 1]
   [(= n 1) var]
   [(odd? n) (* var (power var (sub1 n)))]
   [(even? n) 
    (let ((h (/ n 2)))
     (let ((var^ (power var h)))
      (* var^ var^)))])))

(define powerXpartials
 (lambda (var n)
  (cond
   [(zero? n) (return-writer 1)]
   [(= n 1) (return-writer var)]
   [(odd? n) (bind-writer (powerXpartials var (sub1 n)) (lambda (d) `(,(* var d) . (,d))))]
   [(even? n) 
    (let ((nhalf (/ n 2)))
     (bind-writer (powerXpartials var nhalf) (lambda (d) `(,(* d d) . (,d)))))])))


(define replace-with-count
 (lambda (var lis)
  (cond
   [(null? lis) (return-state '())]
   [(eqv? (car lis) var)
    (bind-state (lambda (s) `(__ . ,(+ s 1)))
                   (lambda (_)
                     (replace-with-count var (cdr lis))))
    (do bind-state
     (s <- get-state)
     (put-state (add1 s))
     (n <- (cons (add1 s) (replace-with-count var (cdr lis))))
      (return-state n))]
   [else
    (bind-state (lambda (s) `(__ . ,(+ s 1)))
                   (lambda (_)
                     (replace-with-count var (cdr lis))))
    (cons (car lis) (replace-with-count var (cdr lis)))])))


(define traverse
 (lambda (return bind f)
  (letrec
   ((trav
    (lambda (tree)
     (cond
      [(pair? tree)
       (do bind
        (a <- (trav (car tree)))
        (d <- (trav (cdr tree)))
        (return (cons a d)))]
      [else (f tree)]))))
   trav)))

(define reciprocal
 (lambda (n)
  (cond
   [(zero? n) (fail)]
   [else (return-maybe (/ 1 n))])))

(define traverse-reciprocal
 (traverse return-maybe bind-maybe reciprocal))


(define halve
 (lambda (n)
  (cond 
   [(even? n) (return-writer (/ n 2))]
   [else (bind-writer (tell-writer n) (lambda (_) (return-writer n)))])))

(define traverse-halve
 (traverse return-writer bind-writer halve))


(define state/sum
 (lambda (n)
  (do bind-state 
   (s <- get-state)
   (put-state (+ n s))
   (return-state s))))

(define traverse-state/sum
 (traverse return-state bind-state state/sum))
 

(define empty-k
 (lambda ()
  (let ((once-only #f))
   (lambda (v)
    (if once-only 
     (error 'empty-k "You can only invoke the empty continuation once")
     (begin (set! once-only #t) v))))))


(define empty-env
 (lambda ()
  (list 'empty-env)))

(define extend-env
 (lambda (x a env)
  (list 'extend-env x a env)))

(define apply-env
 (lambda (env y)
  (match env
   [`(empty-env) (error 'error)]
   [`(extend-env ,x ,a ,env) (if (eqv? y x) (return-cont a) (bind-cont (apply-env env y) (lambda (a) (return-cont a))))])))

(define closure
 (lambda (x body env)
  (list 'closure x body env)))

(define apply-closure
 (lambda (p a)
  (match p
   [`(closure ,x ,body, env) (bind-cont (value-of-cps body (extend-env x a env)) (lambda (a) (return-cont a)))])))

(define fact-5
 '((lambda (f)
   ((f f) 5))
   (lambda (f)
    (lambda (n)
     (if (zero? n)
      1
      (* n ((f f) (sub1 n))))))))

(define capture-fun
 '(* 3 (capture q (* 2 (return 4 q)))))

(define value-of-cps
 (lambda (expr env)
  (match expr
   [`,n #:when (or (number? n) (boolean? n)) (return-cont n)]
   [`(+ ,x1 ,x2) (bind-cont (value-of-cps x1 env) (lambda (x1^) (bind-cont (value-of-cps x2 env) (lambda (x2^) (return-cont (+ x1^ x2^))))))]
   [`(* ,x1 ,x2) (bind-cont (value-of-cps x1 env) (lambda (x1^) (bind-cont (value-of-cps x2 env) (lambda (x2^) (return-cont (* x1^ x2^))))))]
   [`(sub1 ,x) (bind-cont (value-of-cps x env) (lambda (x^) (return-cont (sub1 x^))))]
   [`(zero? ,x) (bind-cont (value-of-cps x env) (lambda (x^) (return-cont (zero? x^))))]
   [`(if ,test ,conseq ,alt) 
    (bind-cont 
     (value-of-cps test env) 
     (lambda (test^) 
      (if test^
       (bind-cont (value-of-cps conseq env) (lambda (a) (return-cont a)))
       (bind-cont (value-of-cps alt env) (lambda (b) (return-cont b))))))]
   [`(capture ,k-id ,body) 
    (bind-cont 
     (callcc 
      (lambda (k) 
       (bind-cont 
        (value-of-cps body (extend-env k-id k env)) (lambda (b) (return-cont b))))) 
      (lambda (a) (return-cont a)))]
   [`(return ,v-exp ,k-exp) (bind-cont (value-of-cps k-exp env) (lambda (k-exp^) (bind-cont (value-of-cps v-exp env) k-exp^)))]
   [`,x #:when (symbol? x) (bind-cont (apply-env env x) (lambda (d) (return-cont d)))]
   [`(lambda (,id) ,body) (return-cont (closure id body env))]
   [`(,rator ,rand)
    (bind-cont 
     (value-of-cps rator env) 
      (lambda (closure^) 
       (bind-cont 
        (value-of-cps rand env) 
         (lambda (arg) (bind-cont (apply-closure closure^ arg) (lambda (h) (return-cont h)))))))])))