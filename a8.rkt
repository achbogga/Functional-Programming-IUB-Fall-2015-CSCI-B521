#lang racket
(define k* #f)
(define v* #f)
(define whokarz1* #f)
(define whokarz2* #f)
(define whokarz3* #f)

(define empty-k
  (lambda ()
    `(empty-k)))

(define ack-k
  (lambda (m k)
    `(ack-k ,m ,k)))
(define apply-k-ack
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(ack-k ,m ,k)
       (begin
         (set! whokarz1* m)
         (set! whokarz2* v*)
         (set! k* k)
         (ack-reg))])))

(define ack-reg
  (lambda ()
    (cond
      [(zero? whokarz1*) (begin (set! v* (add1 whokarz2*))
                          (apply-k-ack))]
      [(zero? whokarz2*) (begin (set! whokarz1* (sub1 whokarz1*))
                          (set! whokarz2* 1)
                          (ack-reg))]
      [else (begin (set! k* (ack-k (sub1 whokarz1*) k*))
                   (set! whokarz2* (sub1 whokarz2*))
                   (ack-reg))])))
(define ack-reg-driver
  (lambda (m n)
    (begin (set! whokarz1* m)
           (set! whokarz2* n)
           (set! k* (empty-k))
           (ack-reg))))


(define big-k-dep
  (lambda (ls k)
    `(big-k-dep ,ls ,k)))
(define small-k-dep
  (lambda (ln k)
    `(small-k-dep ,ln ,k)))
(define apply-k-dep
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(big-k-dep ,ls ,k)
       (begin (set! whokarz1* (cdr ls))
              (set! k* (small-k-dep v* k))
              (depth-reg))]
      [`(small-k-dep ,ln ,k)
       (begin (set! ln (+ ln 1))
              (if (< ln v*)
                  (begin (set! k* k)
                         (apply-k-dep))
                  (begin (set! v* ln)
                         (set! k* k)
                         (apply-k-dep))))])))

(define depth-reg
  (lambda ()
    (cond
      [(null? whokarz1*) (begin (set! v* 1) (apply-k-dep))]
      [(pair? (car whokarz1*))
       (begin (set! k* (big-k-dep whokarz1* k*))
              (set! whokarz1* (car whokarz1*))
              (depth-reg))]
      [else (begin (set! whokarz1* (cdr whokarz1*))
                   (depth-reg))])))

(define depth-reg-driver
  (lambda (ls)
    (begin (set! whokarz1* ls)
           (set! k* (empty-k))
           (depth-reg))))


(define fact-k
  (lambda (n k)
    `(fact-k ,n ,k)))
(define apply-k-fact
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(fact-k ,n ,k)
       (begin (set! v* (* n v*))
              (set! k* k)
              (apply-k-fact))])))
(define fact-reg
  (lambda ()
    (cond
      [(zero? whokarz1*) (begin (set! v* 1)
                          (apply-k-fact))]
      [else (begin (set! k* (fact-k whokarz1* k*))
                   (set! whokarz1* (sub1 whokarz1*))
                   (fact-reg))])))
(define fact-reg-driver
  (lambda (n)
    (begin (set! whokarz1* n)
           (set! k* (empty-k))
           (fact-reg))))



(define pascal-k
  (lambda (a k)
    `(pascal-k ,a ,k)))
(define apply-k-pascal
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(pascal-k ,a ,k)
       (begin (set! v* (cons a v*))
              (set! k* k)
              (apply-k-pascal))])))

(define pascal-reg
  (lambda ()
    (cond
      [(> whokarz2* whokarz1*) (begin (set! v* '())
                          (apply-k-pascal))]
      [else 
       (set! whokarz3* (+ whokarz3* whokarz2*))
       (set! k* (pascal-k whokarz3* k*))
       (set! whokarz2* (+ 1 whokarz2*))
       (pascal-reg)])))
(define pascal-reg-driver
  (lambda (n)
    (begin (set! whokarz1* n)
           (set! whokarz2* 1)
           (set! whokarz3* 0)
           (set! k* (empty-k))
           (pascal-reg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Part 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|(define Procedure_Call* #f)
(define done* #f)
(define rampoline
  (lambda (f1 f2 f3)
    (let ((x (random 3)))
      (cond
        [(= x 0) ((f1) f2 f3)]
        [(= x 1) (f1 (f2) f3)]
        [(= x 2) (f1 f2 (f3))]
        ))))

#(define trampoline
  (lambda ()
    (if done*
        v*
        (begin (Procedure_Call*) (trampoline)))))

#;(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (call/cc
      (lambda (jumpout)
	(rampoline
	    (lambda ()
	      (fib n1 (empty-k jumpout)))
	    (lambda ()
	      (fib n2 (empty-k jumpout)))
            (lambda ()
	      (fib n3 (empty-k jumpout))))))))

(define apply-k-ack-tram
  (lambda ()
    (match k*
      [`(empty-k) (set! done* #t)]
      [`(ack-k ,m ,k)
       (set! whokarz1* m)
       (set! whokarz2* v*)
       (set! k* k)
       (set! Procedure_Call* ack-tram)])))
(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))
(define ack-tram
  (lambda ()
    (cond
      [(zero? whokarz1*) (begin (set! v* (add1 whokarz2*))
                          (set! Procedure_Call* apply-k-ack-tram))]
      [(zero? whokarz2*) (begin (set! whokarz1* (sub1 whokarz1*))
                          (set! whokarz2* 1)
                          (set! Procedure_Call* ack-tram))]
      [else (begin (set! k* (ack-k (sub1 whokarz1*) k*))
                   (set! whokarz2* (sub1 whokarz2*))
                   (set! Procedure_Call* ack-tram))])))
#;(define ack-tramp-driver
  (lambda (m n)
    (begin (set! whokarz1* m)
           (set! whokarz2* n)
           (set! done* #f)
           (set! k* (empty-k))
           (set! Procedure_Call* ack-tram)
           (trampoline))))
(define ack-ramp-driver
  (lambda (n1 n2 n3)
    (call/cc
      (lambda (jumpout)
	(rampoline
	    (lambda ()
	      (ack n1 (empty-k jumpout)))
	    (lambda ()
	      (ack n2 (empty-k jumpout)))
            (lambda ()
	      (ack n3 (empty-k jumpout))))))))
|#



