;;;;;;;;;;;;;;;;;;;;Discussed with debasis dwivedy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "numbers.rkt")
(require "mk.rkt")
;(require "a13-student-tests.rkt")
;(test-file #:file-name "a13.rkt")

(define listo
  (lambda (lis)
    (conde
     ((== lis '()))
     ((=/= lis '())
      (fresh (a d)
             (== `(,a . ,d) lis) (listo d))))))


(define facto
  (lambda (n o)
    (conde
     [(== n '()) (== o (build-num 1))]
     [(fresh (sub1num result)
            (minuso n (build-num 1) sub1num)
            (facto sub1num result)
            (*o result n o))])))


(define fibs
    (lambda (n)
      (cond
        ((eqv? n 0) (values 1 1))
        (else
         (let ((n- (- n 1)))
           (let-values (((u v) (fibs n-)))
             (let ((u+v (+ u v)))
               (values v u+v))))))))

(define fibso
  (lambda (num o1 o2)
    (conde
     [(== num '()) (== o1 (build-num 1)) (== o2 (build-num 1))]
     [(fresh (ad sub1num result1 result2)
             (minuso num (build-num 1) sub1num)
             (fibso sub1num result1 result2)
             (pluso result1 result2 ad)
             (== result2 o1)
             (== ad o2)
             )])))



(define lookupo
  (lambda (x vars valis o)
    (fresh (y vars^ v valis^)
      (== `(,vars . ,valis) `((,y . ,vars^) . (,v . ,valis^)))
      (conde
        ((== x y) (== v o))
        ((=/= x y) (lookupo x vars^ valis^ o))))))

(define valof-o
  (lambda (args vars valis o)
    (conde
      ((== '() args) (== '() o))
      ((fresh (arg args^)
         (== `(,arg . ,args^) args)
         (fresh (v vs)
           (== `(,v . ,vs) o)
           (val-ofo arg vars valis v) 
           (valof-o args^ vars valis vs)))))))

(define fo-lavo-o
  (lambda (args vars valis o)
    (conde
      ((== '() args) (== '() o))
      ((fresh (arg args^)
         (== `(,arg . ,args^) args)
         (fresh (v vs)
           (== `(,v . ,vs) o)
           (fo-lavo arg vars valis v) 
           (fo-lavo-o args^ vars valis vs)))))))

(define val-ofo
  (lambda (exp vars valis o)
    (conde
      [(== `(quote ,o) exp) (absento 'closure o)]
      [(fresh (args)
         (== `(list . ,args) exp)
         (valof-o args vars valis o))]
      ((symbolo exp) (lookupo exp vars valis o))
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (== `(closure ,x ,body ,vars ,valis) o)))
      ((fresh (rator rand)
         (== `(,rator ,rand) exp)
         (fresh (x body vars^ valis^)
           (val-ofo rator vars valis `(closure ,x ,body ,vars^ ,valis^))
           (fresh (a)
             (val-ofo rand vars valis a)
             (val-ofo body `(,x . ,vars^) `(,a . ,valis^) o))))))))

(define fo-lavo
  (lambda (exp vars valis o)
    (conde
      ((symbolo exp) (lookupo exp vars valis o))
      [(== `(,o etouq) exp) 
       (absento 'closure o)
       (absento 'etouq vars)]
      [(fresh (car_a cdr_a args)
         (== `(,car_a ,cdr_a) args)
         (== `(,car_a ,cdr_a tsil) exp)
         (absento 'tsil vars)
         (fo-lavo-o args vars valis o))]
      ((fresh (x body)
         (== `( ,body (,x) adbmal) exp)
         (symbolo x)
	 (absento 'adbmal vars)
         (== `(closure ,x ,body ,vars ,valis) o)))
      ((fresh (rand rator)
         (== `(,rand ,rator) exp)
         (fresh (x body vars^ valis^)
           (fo-lavo rator vars valis `(closure ,x ,body ,vars^ ,valis^))
           (fresh (a)
             (fo-lavo rand vars valis a)
             (fo-lavo body `(,x . ,vars^) `(,a . ,valis^) o))))))))



(define middle-earth
    '((lindon eriador forodwaith)
      (forodwaith lindon rhovanion eriador)
      (eriador lindon forodwaith rhovanion enedwaith)
      (rhovanion forodwaith eriador enedwaith rohan rhun)
      (enedwaith eriador rhovanion rohan gondor)
      (rohan enedwaith rhovanion rhun gondor mordor)
      (gondor enedwaith rohan mordor)
      (rhun rohan rhovanion khand mordor)
      (mordor gondor rohan rhun khand harad)
      (khand mordor rhun harad)
      (harad mordor khand)))


(define membero
  (lambda (x l)
    (conde
      ((== l '()))
      ((fresh (d)
      (== (cons x d) l)))
        ((fresh (d)
          (fresh (a)
           (== (cons a d) l))
          (membero x d))))))

(define color-middle-earth
  (lambda (list)
    (run 1 (q) (color-model list q))))

(define color-model
  (lambda (lis out)
    (fresh (c1 c2 c3 c4 c5 c6 c7 c8 c9 c0 c10)
       (== out `((lindon . ,c0) (forodwaith . ,c1)
                  (eriador . ,c2) (rhovanion . ,c3) (enedwaith . ,c4)
                  (rohan . ,c5) (gondor . ,c6) (rhun . ,c7)
                  (mordor . ,c8) ( khand . ,c9) (harad . ,c10)))
        (absento c0 c2) 
        (absento c0 c1)
        (absento c1 c3)
        (absento c1 c2)
        (absento c2 c3)
        (absento c2 c4)
        (absento c3 c4)
        (absento c3 c5)
        (absento c3 c7)
        (absento c4 c5)
        (absento c4 c6)
        (absento c5 c7) 
        (absento c5 c6) 
        (absento c5 c8)
        (absento c6 c8)
        (absento c7 c9) 
        (absento c7 c8)
        (absento c8 c9)
        (absento c8 c10)
        (absento c9 c10)
        (membero c0 lis)
        (membero c1 lis)
        (membero c2 lis)
        (membero c3 lis)
        (membero c4 lis)
        (membero c5 lis)
        (membero c6 lis)
        (membero c7 lis)
        (membero c8 lis)
        (membero c9 lis)
        (membero c10 lis))))