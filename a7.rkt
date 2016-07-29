;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;high level discussion with debasis dwivedy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;Part 1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(define last-non-zero
  (lambda (ls)
    (call/cc
     (lambda (k)
               (letrec ((last-non-zero
                         (lambda (ls)
                             (cond
                               ((null? ls) '())
                               ((zero? (car ls)) (k (last-non-zero (cdr ls))))
                               (else
                               (cons (car ls) (last-non-zero (cdr ls))))))))
                 (last-non-zero ls))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Part two;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lex
  (lambda (exp acc)
    (match exp
      [`,n #:when (number? n) `(const ,n)]
      [`,b #:when (boolean? b) `(bool ,b)]
      [`,x #:when (symbol? x) (if (memv x acc)
                                   `(var ,(- (length acc) (length (memv x acc))))
                                   `(var 0 ))]
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(sub1 ,arg) `(sub1 ,(lex arg acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(+ ,arg1 ,arg2) `(+ ,(lex arg1 acc) ,(lex arg2 acc))]
      [`(let ([,v ,exp]) ,body) `(let ,(lex exp (cons v acc)) ,(lex body (cons v acc)))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x acc)))]
      [`(if ,test ,conseq ,alt) `(if ,(lex test acc) ,(lex conseq acc) ,(lex alt acc))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]
      [`(capture ,k ,body) `(capture ,(lex body (append (list (cons k 0)) acc)))]
      [`(return ,k-exp ,v-exp) `(return ,(lex k-exp acc) ,(lex v-exp acc))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Part three;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(capture ,body) (call/cc (lambda (k)
                                   (value-of body (lambda (y) (if (zero? y) k (env (sub1 y)))))))]
      [`(return ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
 
(define apply-k
  (λ (k v)
    (k v)))

#;(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env
                                     (λ (v)
                                         (value-of-cps x2 env
                                             (λ (w) (apply-k k (* v w))))))]
      [`(sub1 ,x) (value-of-cps x env
                            (λ(v) (apply-k k (sub1 v))))]
      [`(zero ,x) (value-of-cps x env
                            (λ(v) (apply-k k (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env
                                              (λ (v)
                                                (value-of-cps conseq env
                                                        (λ (w)
                                                          (value-of-cps alt env
                                                              (λ (x)
                                                                (apply-k k (if v w x))))))))]
      [`(capture ,body) (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ k) (env (sub1 y) k^))) k)]
      [`(return ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (v) (value-of-cps v-exp env v)))]
      [`(let ,e ,body) (value-of-cps e env (lambda (a) (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ a) (env (sub1 y) k^))) k)))]
      [`(var ,expr) (apply-k k env expr)]
      [`(lambda ,body) (apply-k k (lambda (a k) (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ a) (env (sub1 y) k^))) k)))]
      [`(app ,rator ,rand) (value-of-cps rator env
                                     (λ(v)
                                       (value-of-cps rand env
                                          (λ(w)
                                       (apply-k k (v w))))))])))
 
(define empty-env
  (lambda (n)
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Part4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))

(define join$
  (lambda (f $ $^)
    (cons$ (f (car$ $) (car$ $^))
     (join$ f (cdr$ $) (cdr$ $^)))))

(define trib$
  (cons$ 0
     (cons$ 1
        (cons$ 1
             (join$ + trib$ (join$ + (cdr$ trib$) (cdr$ (cdr$ trib$))))))))