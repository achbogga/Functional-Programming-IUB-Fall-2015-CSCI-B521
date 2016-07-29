#lang racket
(require racket/trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Empty - k;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Natural Recursion;;;;;;;;;;;;;;;;;;;
(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))
;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (λ (v) (k (+ (car n) (* 2 v)))))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))
;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v)
                                  (k (* (car ls) v))))])))
;(times-cps '(1 2 3 4 5) (empty-k))

;;;;;;;;;;;;;;;;Q3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;CPS - shortcut;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(trace-define times-cps-shortcut;; cool!
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls))  0]
      [else (times-cps (cdr ls) (lambda (v)
                                  (k (* (car ls) v))))])))
;;;;;;;;;;;;;;;;;;;;;Q4;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;
(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))
;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;
(define plus-cps
  (lambda (m k)
    (k (lambda (n k)
         (k (+ m n))))))
;;((plus-cps 2 (empty-k)) 3 (empty-k))

;;;;;;;;;;;;;;;;;;;;;;;;;Q5;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (cond
         [(remv-first-9*-cps (car ls) (lambda (v1) (k (equal? (car ls) v1)))) (remv-first-9*-cps (cdr ls) (λ(v) (k (cons (car ls) v))))]
         [else ((remv-first-9*-cps (car ls) (λ(v)(k cons v (cdr ls)))))])]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (λ(v)(k (cons  (car ls) v))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;Q6;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define count-syms*
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
      [(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
      [else (count-syms* (cdr ls))])))
;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define count-syms*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [(pair? (car ls))
       (count-syms*-cps (car ls)
                    (lambda (v1)
                      (count-syms*-cps (cdr ls)
                                   (lambda (v2)
                                     (k (+ v1 v2))))))]
      [(symbol? (car ls)) (count-syms*-cps (cdr ls)
                                       (lambda (v)
                                         (k (add1 v))))]
      [else (count-syms*-cps (cdr ls)
                         (lambda (v)
                           (k v)))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q7;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))
;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls)
                            (lambda (v1)
                              (cons-cell-count-cps (cdr ls)
                                                   (lambda (v2)
                                                     (k (add1 (+ v1 v2)))))))]
      [else (k 0)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-cps 
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s k) (k u)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q9;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v)
                              (ack-cps (sub1 m) v k)))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q10;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(trace-define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(trace-define fib-cps
  (lambda (n k)
    ((lambda (fib k)
       (fib fib n k))
     (lambda (fib n k)
       (cond
         [(zero? n) (k 0)]
         [(= 1 n) (k 1)]
         [else (fib fib (sub1 n) (lambda (a)
                                   (fib fib (sub1 (sub1 n))
                                        (lambda (d)
                                          (k (+ a d))))))]))
     k)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q11:::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
         (if (p seed)
             ans
             ((h h) (g seed) (cons (f seed) ans))))))))
;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define null?-cps
  (lambda (ls k)
    (k (null? ls))))
(define car-cps
  (lambda (pr k)
    (k (car pr))))
(define cdr-cps
  (lambda (pr k)
    (k (cdr pr))))
(define unfold-cps
  (lambda (p f g seed V)
    ((lambda (h k)
       (h h (lambda (r)
              (r seed '() k))))
     (lambda (h k)
       (k (lambda (seed ans k1)
            (p seed (lambda (t)
                      (if t
                          (k1 ans)
                          (h h (lambda (r)
                                 (g seed (lambda (re)
                                           (f seed (lambda (e)
                                                     (r re (cons e ans) k1)))))))))))))
     V)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q12;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-s
  (lambda ()
    '()))
 
(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) `((,u . ,v) . ,s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k `((,u . ,v) . ,s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify-cps (λ(v) (k (find (car u) v))) (λ(l) (k (find (car v) l))) s)))
             (if s (unify-cps (find (cdr u) s) (find (cdr v) s) s k) #f))
	   #f))
      (else (k #f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        [(null? ls) '()]
        [else (cons (f (car ls)) ((M f) (cdr ls)))]))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
         (cond
           [(null? ls) (k '())]
           [else (f (car ls)
                    (lambda (v1)
                      (M-cps f (lambda (v2)
                                 (v2 (cdr ls)
                                       (lambda (v3)
                                         (k (cons v1 v3))))))))])))))
;;((M-cps car-cps (empty-k)) '((1 2) (3 4) (5 6)) (empty-k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q14;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))
(define use-of-M-cps
  ((M-cps car-cps (empty-k)) '((1 2) (3 4) (5 6)) (empty-k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q15;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define strange-cps
  (lambda (x k)
    ((lambda (g k) (k (lambda (x k) (g g k))))
     (lambda (g k) (k (lambda (x k) (g g k))))
     k)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q16;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))
(define use-of-strange-cps
  ((strange-cps 100 (empty-k)) 100 (empty-k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q17;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;NR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CPS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g (lambda (v) (v x k))))
          k))
     (lambda (g k)
       (f (lambda (x k) (g g (lambda (v) (v x k))))
          k))
     k)))
(define almost-length
  (lambda (f)
    (lambda (ls)
      (if (null? ls)
          0
          (add1 (f (cdr ls)))))))
(define almost-length-cps
  (lambda (f k)
    (k (lambda (ls k)
         (if (null? ls)
             (k 0)
             (f (cdr ls)
                (lambda (v)
                  (k (add1 v)))))))))