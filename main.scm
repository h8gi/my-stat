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
    (cond ((null? lst) (values sum n (if (zero? n) 0 (/ sum n))))
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

;;; sampling
(define (sample lst n #!key (replace #t))
  (define (delete-1 x lst acc)
    (cond [(null? lst) '()]
          [(equal? x (car lst)) (append acc (cdr lst))]
          [else (delete-1 x (cdr lst) (cons (car lst) acc))]))
  (define (sample-replace len acc)
    (map (lambda (x) (list-ref lst (random len)))
         (make-list n)))
  (define (sample-not lst len n acc)
    (cond [(or (zero? n) (null? lst)) acc]
          [else (let ([val (list-ref lst (random len))])
                  (sample-not (delete-1 val lst '())
                              (sub1 len)
                              (sub1 n)
                              (cons val acc)))]))
  (if replace
      (sample-replace (length lst) '())
      (sample-not lst (length lst) n '())))
