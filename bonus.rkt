;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;High Level Discussion with Debasis Dwivedy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define filter-sps
 (lambda (p lis store)
  (cond
   [(null? lis) (values '() '())]
   [else
    (let-values (((arg  store) (filter-sps p (cdr lis) store)))
      (cond
        ((p (car lis)) (values (cons (car lis) arg) store))  
        (else
         (values arg (cons (car lis) store)))))])))

(define filter*
  (lambda (f lis)
    (cond
      [(null? lis) '()]
      [(pair? (car lis))
       (cons (filter* f (car lis)) (filter* f (cdr lis)))]
      [(null? (car lis)) '()]
      [(f (car lis)) (cons (car lis) (filter* f (cdr lis)))]
      [else (filter* f (cdr lis))])))


(define filter*-sps
  (lambda (p lis store)
    (cond
    [(null? lis) (values '() '())]
    [(pair? (car lis))
       (let*-values (((left store1)(filter*-sps p (car lis) '()))
                     ((right store2)(filter*-sps p (cdr lis) store)))
         (values (cons left right) (append store1 store2)))]
    [else
    (let-values (((arg  store) (filter*-sps p (cdr lis) store)))
      (cond
        ((p (car lis)) (values (cons (car lis) arg) store))   
        (else
         (values arg (cons (car lis) store)))))])))
    
    




(define fib-sps
  (lambda (g store)
    (cond
      ((assv g store) =>
       (lambda (p) (values (cdr p) store)))
      ((zero? g) (values g `((,g . 0) . ,store))) 
      ((= 1 g) (values g `((,g . 1) . ,store))) 
      (else
       (let*-values (((u store) (fib-sps (sub1 (sub1 g)) store))
                     ((v store) (fib-sps (sub1 g) store)))
         (values (+ u v) `((,g . ,(+ u v)) . ,store)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Macros;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax and*
  (syntax-rules()
    ((_) #t)
    ((_ v0) (if (or (number? v0) (symbol? v0)) v0 (if v0 #t #f)))
    ((_ v0 v1 ...) (and* v1 ...))))
(define-syntax cons*
  (syntax-rules()
    ((_)  (syntax-error “Incorrect argument-count to cons*”))
    ((_ v0) v0)
    ((_ v0 v1) (cons v0 v1))
    ((_ v0 v1 v2 ...) (cons v0 (cons* v1 v2 ...)))))
(define-syntax macro-list
  (syntax-rules()
    ((_)  '())
    ((_ v0) `(,v0))
    ((_ v0 v1 ...) (cons v0 (macro-list v1 ...)))
    ))
(define-syntax mcond
  (syntax-rules(else)
    ((_
      (t0 b0 ...)
      (else b1 ...)
      ) (if t0 b0 ... b1 ...))
    ((_
      (else b1 ...)
      ) (if #f b1 ... b1 ...))
    ((_
      (t0 b0 ...)) (if t0 b0 ... #f))
    ((_
      (t0 b0 ...)
      (t1 b1 ...)
      ...
      ) (if t0 b0 ... (mcond
                       (t1 b1 ...)
                       ...)))
    ((_
      (t0 b0 ...)
      (t1 b1 ...)
      ...
      (else b2 ...)
      ) (if t0 b0 ... (mcond
                       (t1 b1 ...)
                       ...
                       (else b2 ...))))))
(define-syntax macro-map
  (syntax-rules()
    ((_ f '()) '())
    ((_ f '(b0)) `(,(f b0)))
    ((_ f '(b0 b1 ...)) `(,(f b0) ,(macro-map f '(b1 ...))))))
        

