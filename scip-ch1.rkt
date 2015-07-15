#lang slideshow

; Newton's method

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (let ([diff (abs (- (square guess) x))])
    (println (exact->inexact diff))
    (< diff 0.001)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y)
     2))

; Compute change

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Pascal's triangle
; 1
; 1   1
; 1   2   1
; 1   3   3    1
; 1   4   6    4   1

(define (map-next-pascal prev next i)
    (if (< i (length prev))
      (cond ((= i 0)
             (map-next-pascal prev
                              (cons 1 next)
                              (+ i 1)))
            (else
             (map-next-pascal prev
                              (cons (+ (list-ref prev i) (list-ref prev (- i 1)))
                                    next)
                              (+ i 1))))
      (cons 1 next)))

(define (print-pascal level)
  (pascal '(1) 0 level))

(define (pascal curr i max)
    (when (< i max)
        (let* ([next (map-next-pascal curr '() 0)])
          (writeln curr)
          (pascal next (+ i 1) max))))

; Exponentiation by succesive squares

 (define (iter-expt a b n)
    (cond ((= n 0) a)
          ((odd? n) (iter-expt (* a b) (square b) (quotient n 2)))
          (else
           (iter-expt a (square b) (quotient n 2)))))

(define (recursive-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (recursive-expt b (/ n 2))))
        (else (* b (recursive-expt b (- n 1))))))

(define (linear-expt b n)
  (cond ((= n 0) 1)
        (else (* b (expt b (- n 1))))))

