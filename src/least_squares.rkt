#lang racket
;TODO
;use matrix decomposition for betahat
;use t distribution instead of normal

(require math/matrix
         math/array
         math/distributions
         math/statistics
         plot)

(provide betahat
         intercept
         coefs
         regression-equation
         fit-ys
         residual-sum-of-squares
         degrees-of-freedom
         residual-variance
         r-square
         se-coef
         confidence-interval
         t-stat
         p-value
         plot-least-squares)

(define (betahat x y)
  ;must be redone with matrix decomposition, see Faraway p.19
  (matrix->list
   (matrix*
    (matrix-inverse (matrix* x (matrix-transpose x)))
    x
    (matrix-transpose y))))

(define (intercept bh)
  (list-ref bh 0))

(define (coefs bh)
  (cdr bh))

(define (regression-equation intercept coefs)
  (λ (x) (+ intercept
            (foldl + 0
             (map (λ (b) (* b x)) coefs)))))

(define (fit-ys intercept coefs xs)
  (array-map (regression-equation intercept coefs) xs))

(define (residual-sum-of-squares fit data)
  (array-all-sum (array-sqr (array- fit data))))

(define (degrees-of-freedom n p)
  (- n p))

(define (residual-variance rss df)
  (/ rss df))

(define (r-square rss y)
  (let ([mu (/ (array-all-sum y) (array-size y))])
    (- 1 (/ rss (array-all-sum (array-sqr (array-map (λ (e) (- e mu)) y)))))))

(define (se-coef x sigma n)
  (*  sigma
      (sqrt
       (matrix-ref
        (matrix-inverse (matrix* x (matrix-transpose x))) n n))))

(define (confidence-interval coef se-coef alpha [sides 2])
  (map (λ(b) (+ coef (* b se-coef)))
         (map (λ(x) (inv-cdf (normal-dist) x))
              (list (/ alpha sides) (- 1 (/ alpha sides))))))

(define (t-stat coef se-coef [hypothesis 0])
  ;hypothesis default value is 0, since we usually
  ;want to see if coef is \= from 0, however this
  ;leaves freedom for other hypotheses.
  (/ (- coef hypothesis) se-coef))

(define (p-value t-stat [sides 2])
  (* sides (cdf (normal-dist) (* -1 (abs t-stat)))))

(define (plot-least-squares intercept coefs xs ys)
  (let ([pts
         (matrix->vector*
          (matrix-transpose
           (matrix-stack (list xs ys))))])
  (plot
   (list
    (function (regression-equation intercept coefs)
              0 (+ 1 (array-size xs)))
    (points pts)))))