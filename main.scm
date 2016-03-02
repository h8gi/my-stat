(use random-mtzig matchable)
;;; random
(define pi (acos -1))
(define uniform
  (let ([st (init)])
    (lambda () (randu! st))))

(define (range-random a b)
  (+ a (* (- b a)
          (uniform))))

(define (random-normal #!optional (mean 0) (sd 1))
  (+ mean (* sd
             (* (sqrt (* -2 (log (uniform))))
                (cos  (* 2 pi (uniform)))))))

;;; statistics
(define (sum-n-mean lst)
  (define (inner lst sum n)
    (cond ((null? lst) (values sum n (/ sum n)))
          (else (inner (cdr lst) (+ (car lst) sum) (add1 n)))))
  (inner lst 0 0))
(define (sum lst)
  (define (inner lst s)
    (cond ((null? lst) s)
          (else (inner (cdr lst) (+ (car lst) s)))))
  (inner lst 0))

(define (mean lst)
  (let-values ([(s n m) (sum-n-mean lst)])
    m))

(define (sq x) (* x x))

(define ** expt)

(define (sum-f f . lsts)
  (apply fold
         (lambda pat
           (match pat
             [(x ... acc)
              (+ (apply f x) acc)]))
         0 lsts))

(define (sample-variance lst)           ; 標本分散
  (let-values ([(s n m) (sum-n-mean lst)])
    (/ (sum-f (lambda (x) (sq (- m x))) lst)
       n)))

(define (unbiased-variance lst)         ; 不偏分散
  (let-values ([(s n m) (sum-n-mean lst)])
    (/ (sum-f (lambda (x) (sq (- m x))) lst)
       (- n 1))))

(define var unbiased-variance)
(define (sd lst)
  (sqrt (var lst)))

