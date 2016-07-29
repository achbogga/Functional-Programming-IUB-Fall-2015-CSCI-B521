;;(require "a4-student-tests.rkt")
;;(test-file #:file-name "a4.rkt")
#lang racket
(define lex
  (lambda (exp acc)
    (match exp
      [`,n #:when (number? n) `(const ,n)]
      [`,b #:when (boolean? b) `(bool ,b)]
      [`,x #:when (symbol? x) (if (memv x acc)
                                   `(var ,(- (length acc) (length (memv x acc))))
                                   `(free ,x ))]
      [`(zero? ,arg) `(zero? ,(lex arg acc))]
      [`(sub1 ,arg) `(sub1 ,(lex arg acc))]
      [`(* ,arg1 ,arg2) `(* ,(lex arg1 acc) ,(lex arg2 acc))]
      [`(+ ,arg1 ,arg2) `(+ ,(lex arg1 acc) ,(lex arg2 acc))]
      [`(let ([,v ,exp]) ,body) `(let ,(lex exp (cons v acc)) ,(lex body (cons v acc)))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x acc)))]
      [`(if ,test ,conseq ,alt) `(if ,(lex test acc) ,(lex conseq acc) ,(lex alt acc))]
      [`(,rator ,rand) (list (lex rator acc) (lex rand acc))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Part 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;My Environment

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


(define closure-fn
  (lambda (x body env)
    (lambda (arg)
         (value-of-fn body (extend-env x arg env)))))
(define apply-closure-fn
  (lambda (clo arg)
    (clo arg)))


(define closure-ds
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-closure-ds
  (lambda (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (value-of-ds body (extend-env x arg env))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,z #:when (boolean? z) z]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(zero? ,arg) (zero? (value-of-fn arg env))]
      [`(sub1 ,arg) (sub1 (value-of-fn arg env))]
      [`(* ,arg1 ,arg2) (* (value-of-fn arg1 env) (value-of-fn arg2 env))]
      [`(let ([,var ,exp]) ,body) (value-of-fn body
                                               (extend-env var
                                                              (value-of-fn exp env) env))]
      [`(lambda(,x) ,b) (closure-fn x b env)]
      [`(,test ,conseq ,alt) ((value-of-fn test env) (value-of-fn conseq env) (value-of-fn alt env))]
      [`(if ,test ,conseq ,alt) (if (value-of-fn test env) (value-of-fn conseq env) (value-of-fn alt env))]
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env)(value-of-fn rand env))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,z #:when (boolean? z) z]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(zero? ,arg) (zero? (value-of-ds arg env))]
      [`(sub1 ,arg) (sub1 (value-of-ds arg env))]
      [`(* ,arg1 ,arg2) (* (value-of-ds arg1 env) (value-of-ds arg2 env))]
      [`(let ([,var ,exp]) ,body) (value-of-ds body
                                               (extend-env var
                                                              (value-of-ds exp env) env))]           
      [`(lambda(,x) ,b) (closure-ds x b env)]
      ;[`(,test ,conseq ,alt) ((value-of-ds test env) (value-of-ds conseq env) (value-of-ds alt env))]
      [`(if ,test ,conseq ,alt) (if (value-of-ds test env) (value-of-ds conseq env) (value-of-ds alt env))]
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env)(value-of-ds rand env))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define closure-dynamic
  (lambda (x body env)
    `(closure ,x ,body)))
(define apply-closure-dynamic
  (lambda (closure arg env)
    (match closure
      [`(closure ,x ,body)
       (value-of-dynamic body (extend-env x arg env))])))


(define value-of-dynamic
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (apply-env env x)]
      [`(quote ()) '()]
      [`(null? ,ls) 
       (equal? (value-of-dynamic ls env) '())]
      [`(cons ,a ,d) 
       (cons (value-of-dynamic a env) (value-of-dynamic d env))]
      [`(car ,ls) 
       (car (value-of-dynamic ls env))]
      [`(cdr ,ls)
       (cdr (value-of-dynamic ls env))]
      [`(zero? ,arg) (zero? (value-of-dynamic arg env))]
      [`(* ,nexp1 ,nexp2) 
       (* (value-of-dynamic nexp1 env) 
          (value-of-dynamic nexp2 env))]
      [`(sub1 ,nexp) 
       (sub1 (value-of-dynamic nexp env))]
      [`(if ,t ,c ,a) 
       (if (value-of-dynamic t env)
           (value-of-dynamic c env)
           (value-of-dynamic a env))]
      [`(let ((,x ,e)) ,body)
       (let ((a (value-of-dynamic e env)))
         (value-of-dynamic body (extend-env x a env)))]
      #;[`(lambda (,x) ,body) 
       (λ (a env)
         (value-of-dynamic body (extend-env x a env)))]
      [`(lambda(,x) ,b) (closure-dynamic x b env)]
      [`(,rator ,rand) (apply-closure-dynamic (value-of-dynamic rator env) (value-of-dynamic rand env) env)])))    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-env-fn empty-env)
(define empty-env-ds empty-env)
(define extend-env-fn extend-env)
(define extend-env-ds extend-env)
(define apply-env-fn apply-env)
(define apply-env-ds apply-env)

(define closure-fn-ri
  (lambda (x body env fun)
    (lambda (a e)
      ((fun (e x a env)) body))))
(define apply-closure-fn-ri
  (lambda (clos a extend-env)
    (clos a extend-env)))
(define closure-ds-ri
  (lambda (x body env fun)
    (lambda (a e)
      ((fun (e x a env)) body))))

(define apply-closure-ds-ri
  (lambda (clos a extend-env)
    (clos a extend-env)))

(define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (letrec ([loop (lambda (env)
                     (lambda (exp)
                       (match exp
                         [`,n #:when (number? n) n]
                         [`,b #:when (boolean? b) b]
                         [`,x #:when (symbol? x) (apply-env env x)]
                         [`(* ,op1 ,op2)
                          (* ((loop env) op1) 
                             ((loop env) op2))]
                         [`(sub1 ,op)
                          ((loop env) op)]
                         [`(zero? ,op)
                          (zero? ((loop env) op))]
                         [`(if ,t ,c ,a)
                          (if ((loop env) t)
                              ((loop env) c)
                              ((loop env) a))]
                         [`(let ([,x ,e])
                             ,body)
                          (let ([val ((loop env) e)])
                            ((loop (extend-env x val)) body))]
                         [`(lambda (,x) ,body)
                          (closure x body env loop)]
                         [`(,rator ,rand)
                          (apply-closure 
                           ((loop env) rator)
                           ((loop env) rand)
                           extend-env)])))])
      (loop (empty-env)))))