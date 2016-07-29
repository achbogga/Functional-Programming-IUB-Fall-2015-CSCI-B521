#lang racket
(define empty-env
  (lambda ()
    '()))
(define extend-env
  (lambda (var val env)
    `((,var . ,val) . ,env)))
(define apply-env
  (lambda (env var)
    (cond
      [(assq var env) => cdr]
      [else (error 'env "unbound variable. ~s" var)])))


(define closure
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure
  (lambda (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (value-of body (extend-env x arg env))])))


(define value-of
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      [`(random ,n) (random (value-of n env))]
      [`,x #:when (symbol? x) (apply-env env x)]
      [`(lambda (,x) ,body) (closure x body env)]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))



(define empty-env-cbv
  (lambda ()
    '()))
(define extend-env-cbv
  (lambda (var val env)
    `((,var . ,(box val)) . ,env)))
(define apply-env-cbv
  (lambda (env var)
    (cond
      [(assq var env) => (lambda (p) (unbox (cdr p)))]
      [else (error 'env "unbound variable. ~s" var)])))
(define apply-env-cbv-set!  
  (lambda (env var)
    (cond
      [(assq var env) => cdr]
      [else (error 'env "unbound variable. ~s" var)])))

(define closure-cbv
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure-cbv       
  (lambda (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (val-of-cbv body (extend-env-cbv x arg env))])))

(define unbox/cons
  (lambda (b)
    (let ([res ((unbox b))])
      (set-box! b (lambda () res))
      res)))


(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (apply-env-cbv env x)]
      [`(quote ()) '()]
      [`(null? ,ls) (null? (val-of-cbv ls env))]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(let ([,x ,e])
          ,body)
       (val-of-cbv body (extend-env-cbv x (val-of-cbv e env) env))]
      [`(cons^ ,a ,d)
       (cons (box (lambda () (val-of-cbv a env)))
             (box (lambda () (val-of-cbv d env))))]
      [`(car^ ,ls)
       (unbox/cons (car (val-of-cbv ls env)))]
      [`(cdr^ ,ls)
       (unbox/cons (cdr (val-of-cbv ls env)))]
      [`(cons ,a ,d)
       (cons (val-of-cbv a env) (val-of-cbv d env))]
      [`(car ,ls)
       (car (val-of-cbv ls env))]
      [`(cdr ,ls)
       (car (val-of-cbv ls env))]
      [`(set! ,var ,rhs)
       (let ([val (val-of-cbv rhs env)]
             [var (apply-env-cbv-set! env var)])
         (set-box! var val))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(lambda (,x) ,body) (closure-cbv x body env)]
      [`(,rator ,rand) (apply-closure-cbv
                        (val-of-cbv rator env)
                        (val-of-cbv rand env))])))





(define empty-env-cbr  
  (lambda ()
    '()))
(define extend-env-cbr
  (lambda (var val env)
    (if (or (box? val) #;(list? val))    
        `((,var . ,val) . ,env)      
        `((,var . ,(box val)) . ,env))))
(define apply-env-cbr
  (lambda (env var)
    (cond
      [(assq var env) => (lambda (p) (unbox (cdr p)))]
      [else (error 'env "unbound variable. ~s" var)])))
(define apply-env-cbr-box
  (lambda (env var)
    (cond
      [(assq var env) => cdr]
      [else (error 'env "unbound variable. ~s" var)])))
 
(define closure-cbr
  (lambda (x body env)
    `(closure ,x ,body ,env)))
#;
(define apply-closure-cbr       
  (lambda (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (val-of-cbr body (extend-env-cbr x arg env))])))
(define apply-closure-cbr-im       
  (lambda (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (val-of-cbr-im body (extend-env-cbr x arg env))])))



(define unpack (lambda (v)
                 (if (box? v)
                     (unbox v)
                     v)))
(define val-of-cbr-im
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (apply-env-cbr-box env x)]
      [`(zero? ,n) 
       (zero? (unpack (val-of-cbr-im n env)))]
      [`(sub1 ,n) (sub1 (unpack (val-of-cbr-im n env)))]
      [`(* ,n1 ,n2) (* (unpack (val-of-cbr-im n1 env)) (unpack (val-of-cbr-im n2 env)))]
      [`(if ,test ,conseq ,alt) (if (unpack (val-of-cbr-im test env))
                                    (val-of-cbr-im conseq env)
                                    (val-of-cbr-im alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr-im e1 env) (val-of-cbr-im e2 env))]
      [`(set! ,var ,rhs)
       (let ([val (unpack (val-of-cbr-im rhs env))]
             [var (apply-env-cbr-box env var)])
         (set-box! var val))]
      [`(random ,n) (random (unpack (val-of-cbr-im n env)))]
      [`(lambda (,x) ,body) (closure-cbr x body env)]
      [`(,rator ,x) #:when (symbol? x)
                    (apply-closure-cbr-im
                     (unpack (val-of-cbr-im rator env))
                     (apply-env-cbr-box env x))]
      [`(,rator ,rand) (apply-closure-cbr-im
                        (unpack (val-of-cbr-im rator env))
                        (val-of-cbr-im rand env))])))
(define val-of-cbr (lambda (exp env) (unpack (val-of-cbr-im exp env))))


(define closure-cbname
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure-cbname
  (lambda (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (val-of-cbname body (extend-env x arg env))])))


(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) ((unbox (apply-env env x)))]   
      [`(zero? ,n) (zero? (val-of-cbname n env))] 
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (closure-cbname x body env)]
      [`(,rator ,rand) (apply-closure-cbname
                        (val-of-cbname rator env)
                        (box (lambda () (val-of-cbname rand env))))])))

(define closure-cbneed
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure-cbneed
  (lambda (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (val-of-cbneed body (extend-env x arg env))])))


(define unbox/need
  (lambda (b)
    (let ([res ((unbox b))])
      (set-box! b (lambda () res))
      res)))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (unbox/need (apply-env env x))]  
      [`(zero? ,n) (zero? (val-of-cbneed n env))]           
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(lambda (,x) ,body) (closure-cbneed x body env)]
      [`(,rator ,rand) (apply-closure-cbneed
                        (val-of-cbneed rator env)
                        (box (lambda () (val-of-cbneed rand env))))])))