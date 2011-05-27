#lang racket
(require racket/stream)

(define really-big 5000)

(define (divides? x y)
  (= 0 (modulo y x) ))

(define (primes)
  (define (sieve n primes)
    (if (not (ormap (lambda (x) (divides? x n)) primes))
        (stream-cons n (sieve (+ n 2) (append primes (list n))))
        (sieve (+ n 2) primes)))
  (stream-cons 2 (stream-cons 3 (sieve 5 '(2 3)))))

(define (prime-factors n [remaining-primes (primes)])
  (let ([prime (stream-first remaining-primes)])
    (cond
     [(= n 1) null]
     [(divides? prime n)
      (cons prime (prime-factors (/ n prime) remaining-primes))]
     [else (prime-factors n (stream-rest remaining-primes))])))

(define (count-groups seq)
  (define (iterate n cur rest)
    (cond
     [(null? rest) (list n)]
     [(= cur (car rest)) (iterate (+ 1 n) cur (cdr rest))]
     [else (cons n (iterate 1 (car rest) (cdr rest)))]))
  (if (null? seq)
      '()
      (iterate 1 (car seq) (cdr seq))))

(define (n-divisors n)
  (foldl * 1 (map add1 (count-groups (prime-factors n)))))

(define (in-triangles)
  (define (generate n i)
    (stream-cons n (generate (+ n i) (+ i 1))))
  (generate 1 2))

(define (problem-12 [n 500])
  (for/first ([i (in-triangles)]
              #:when (> (n-divisors i) n))
             i))
