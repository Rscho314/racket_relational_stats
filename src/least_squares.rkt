#lang racket

(require math/matrix
         math/array
         plot)
(provide betahat
         intercept
         coefs
         regression-equation
         fit-ys
         residual-sum-of-squares
         degrees-of-freedom
         sd-beta
         plot-least-squares)

(define (betahat x y)
  ;must be redone with matrix decomposition, see Faraway p.19
  (matrix*
   (matrix-inverse (matrix* x (matrix-transpose x)))
   x
   (matrix-transpose y)))

(define (intercept bh)
  (array-ref (array-slice-ref bh '((0)(0))) #(0 0)))

(define (coefs bh)
  (array-flatten (array-slice-ref bh (list (:: 1 #f 1) (::)))))

(define (regression-equation intercept coefs)
  (λ (x) (+ intercept
            (array-all-sum
             (array-map (λ (b) (* b x)) coefs)))))

(define (fit-ys intercept coefs xs)
  (array-map (regression-equation intercept coefs) xs))

(define (residual-sum-of-squares fit data)
  (array-all-sum (array-sqr (array- fit data))))

(define (degrees-of-freedom n p)
  (- n p))

(define (sd-beta rss df)
  (/ rss df))

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