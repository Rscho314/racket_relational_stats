#lang racket

(require rackunit
         rackunit/text-ui
         math/matrix
         math/array
         "../src/least_squares.rkt"
         "../src/data_importer.rkt")

(define data (make-data-hash "../resource/data.csv"))

(define bh
     (betahat (hash-ref data 'x) (hash-ref data 'y)))

(define fits (fit-ys (intercept bh)
                     (coefs bh)
                    (array-flatten
                   (array-slice-ref
                    (hash-ref data 'x)
                    (list (:: 1 #f 1) (::))))))

(define rss (residual-sum-of-squares fits (hash-ref data 'y)))

(define df (degrees-of-freedom (array-size (hash-ref data 'y))
                               (matrix-num-rows (hash-ref data 'x))))

(define residual-sd (sqrt (residual-variance rss df)))

(define rsq (r-square rss (hash-ref data 'y)))

(define se-coefs (map
                  (λ (n) (se-coef (hash-ref data 'x) residual-sd n))
                  (range 0 (length bh))))

(define ci (map (λ(c se) (confidence-interval c se 0.05)) bh se-coefs))

(define t-stats (map (λ(c se) (t-stat c se)) bh se-coefs))

(define p-values (map (λ(t) (p-value t)) t-stats))

;test
(define tests
  (test-suite
   "least squares test suite"

   (check-equal? bh '(7/2 7/5))
   (check-equal? fits (array #[49/10 63/10 77/10 91/10]))
   (check-equal? rss 21/5)
   (check-equal? df 2)
   (check-= residual-sd 1.44 0.01)
   (check-equal? rsq 7/10)
   (map (λ (x y) (check-= x y 0.01)) se-coefs '(1.77 0.64))
   (map (λ (x y) (check-= x y 0.01)) (flatten ci) '(0.021 6.97 0.12 2.67))
   (map (λ (x y) (check-= x y 0.01)) t-stats '(1.97 2.16))
   (map (λ(x y) (check-= x y 0.01)) p-values '(0.048 0.030))
   ))
   
   (plot-least-squares (intercept bh)
                       (coefs bh)
                       (array-slice-ref
                        (hash-ref data 'x) (list (:: 1 2 1) (::)))
                       (hash-ref data 'y))
(run-tests tests)

