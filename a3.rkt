#lang racket

;(require "a3-student-tests.rkt")
;(test-file #:file-name "a3.rkt")

(define apply-begin
  (lambda (b)
    (match b
      [`(begin ,exp1 ,exp2 ,env) #:when (or (boolean? exp1) (number? exp1)) (value-of exp2 env)]
      [`(begin ,exp1 ,exp2 ,env) (apply-begin `(begin ,(value-of exp1 env) ,exp2 ,env))])))   
      

(define value-of
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,z #:when (boolean? z) z]
      [`,y #:when (symbol? y) (env y)]
      [`(zero? ,arg) (zero? (value-of arg env))]
      [`(sub1 ,arg) (sub1 (value-of arg env))]
      [`(* ,arg1 ,arg2) (* (value-of arg1 env) (value-of arg2 env))]
      [`(+ ,arg1 ,arg2) (+ (value-of arg1 env) (value-of arg2 env))]
      [`(let ([,v ,exp]) ,body) (value-of body
                                          (lambda (id)
                                            (if (eq? id v) (value-of exp env) (env id))))]
      [`(begin2 ,exp1 ,exp2) (begin (value-of exp1 env) (value-of exp2 env))]
      ;[`(begin2 ,exp1 ,exp2) (apply-begin `(begin exp1 exp2 env))]
      [`(set! ,v ,exp) (set! v (value-of exp env))]
      [`(lambda(,x) ,b) (lambda(a)
                          (value-of b (lambda(y) (if (eqv? x y) a (env y)))))]
      [`(if ,test ,conseq ,alt) (if (value-of test env) (value-of conseq env) (value-of alt env))]
      [`(,rator ,rand) ((value-of rator env)(value-of rand env))])))


(define empty-env-fn
  (lambda()
    (lambda(var)
      (error 'empty-env-fn "unbound variable ~s" var ))))

(define apply-env-fn
  (lambda(env y)
    (env y)))
(define extend-env-fn
  (lambda (x a env)
    (lambda (y) (if (eqv? x y) a (apply-env-fn env y)))))

(define closure-fn
  (lambda (x body env)
    (lambda (a)
      (value-of-fn body
                   (extend-env-fn x a env)))))

(define closure-ds
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure-ds
  (lambda (cl)
    (match cl
      [`(closure ,x ,body ,env) (lambda (a) (value-of-ds body (extend-env-ds x a env)))])))


(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,z #:when (boolean? z) z]
      ;[`(pred ,arg) #| #:when (or (eqv? pred eqv?) (eqv? pred zero?) (eqv? pred and) (eqv? pred symbol?) (eqv? pred or) (eqv? pred sub1))|# (pred (value-of-fn arg env))]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      [`(zero? ,arg) (zero? (value-of-fn arg env))]
      [`(sub1 ,arg) (sub1 (value-of-fn arg env))]
      [`(* ,arg1 ,arg2) (* (value-of-fn arg1 env) (value-of-fn arg2 env))]
      [`(let ([,var ,exp]) ,body) (value-of-fn body
                                               (extend-env-fn var
                                                              (value-of-fn exp env) env))]
      [`(lambda(,x) ,b) (closure-fn x b env)]
      [`(,test ,conseq ,alt) ((value-of-fn test env) (value-of-fn conseq env) (value-of-fn alt env))]
      [`(if ,test ,conseq ,alt) (if (value-of-fn test env) (value-of-fn conseq env) (value-of-fn alt env))]
      [`(,rator ,rand) ((value-of-fn rator env)(value-of-fn rand env))])))

;;((lambda(x) (x x))(lambda(x) (x x)))

(define extend-env-ds
  (lambda (x a env)
    `(extend-env ,x ,a ,env)))
(define empty-env-ds
  (lambda ()
    `(empty-env)))
(define apply-env-ds
  (lambda (env y)
    (match env
      [`(extend-env ,x ,a ,env) (if(eqv? y x) a (apply-env-ds env y))]
      [`(empty-env) (error 'value-of-ds "unbound variable ~s" y)])))
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,z #:when (boolean? z) z]
      ;[`(pred ,arg) #| #:when (or (eqv? pred eqv?) (eqv? pred zero?) (eqv? pred and) (eqv? pred symbol?) (eqv? pred or) (eqv? pred sub1))|# (pred (value-of-ds arg env))]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`(zero? ,arg) (zero? (value-of-ds arg env))]
      [`(sub1 ,arg) (sub1 (value-of-ds arg env))]
      [`(* ,arg1 ,arg2) (* (value-of-ds arg1 env) (value-of-ds arg2 env))]
      [`(let ([,var ,exp]) ,body) (value-of-ds body
                                               (extend-env-ds var
                                                              (value-of-ds exp env) env))]           
      [`(lambda(,x) ,b) (apply-closure-ds (closure-ds x b env))]
      ;[`(,test ,conseq ,alt) ((value-of-ds test env) (value-of-ds conseq env) (value-of-ds alt env))]
      [`(if ,test ,conseq ,alt) (if (value-of-ds test env) (value-of-ds conseq env) (value-of-ds alt env))]
      [`(,rator ,rand) ((value-of-ds rator env)(value-of-ds rand env))])))

(define apply-env-fn-fo-eulav
  (lambda(env y)
    (env y)))
(define extend-env-fn-fo-eulav
  (lambda (x a env)
    (lambda (y) (if (eqv? x y) a (apply-env-fn-fo-eulav env y)))))

(define closure-fn-fo-eulav
  (lambda (x body env)
    (lambda (a)
      (fo-eulav body
                   (extend-env-fn-fo-eulav x a env)))))

(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,x #:when (number? x) x]
      [`,x #:when (symbol? x) (apply-env-fn-fo-eulav env x)]
      [`(,arg ?orez) (zero? (fo-eulav arg env))]
      [`(,arg 1bus) (sub1 (fo-eulav arg env))]
      [`(,arg1 ,arg2 *) (* (fo-eulav arg1 env) (fo-eulav arg2 env))]
      [`(,alt ,conseq ,test fi) (if (fo-eulav test env) (fo-eulav conseq env) (fo-eulav alt env))]
      [`(,body (,x) adbmal) (closure-fn-fo-eulav x body env)]
      [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))])))



(define value-of-lex
  (lambda (exp env)
    (match exp
      ((? boolean?) exp)
      ((? number?) exp)
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(zero? ,body) (zero? (value-of-lex body env)))
      (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))
(define extend-env-lex
  (lambda (a env)
    (cons a env)))
(define apply-env-lex
  (lambda (env num)
    (cond
      [(zero? num) (car env)]       
      [else (apply-env-lex (cdr env) (- num 1))])))


;(require "a3-student-tests.rkt")
