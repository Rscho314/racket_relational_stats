#lang racket

(require math/statistics
         math/flonum)

(provide regression-equation
         fit-ys
         residual-sum-of-squares
         negative-log-likelihood
         maxll-beta
         maxll-intercept
         maxll-variance)

(define (regression-equation intercept coefs)
  (λ (x) (+ intercept
            (foldl + 0
             (map (λ (b) (* b x)) coefs)))))

(define (fit-ys intercept coefs xs)
  (flvector-map (regression-equation intercept coefs) xs))

(define (residual-sum-of-squares fit data)
  (flvector-sum (flvector-sqr (flvector- fit data))))

(define (negative-log-likelihood xs ys intercept coefs variance)
  (letrec ([fits (fit-ys intercept coefs xs)]
           [rss (residual-sum-of-squares fits ys)]
           [n (flvector-length ys)])
    (* -1 (- (* (/ n -2) (log (* 2 pi)))
       (* (/ n 2) (log variance))
       (*(/ 1 (* 2 variance)) rss)))
    ))

(define (maxll-beta xs ys)
  (letrec ([n (flvector-length ys)]
           [x-normalized (flvector-map (λ(e) (- e (mean xs))) xs)]
           [y-normalized (flvector-map (λ(e) (- e (mean ys))) ys)])
    (/ (flvector-sum (flvector* x-normalized y-normalized))
       (flvector-sum (flvector-sqr x-normalized)))
    ))

(define (maxll-intercept xs ys betamax-mean)
  (- (mean ys) (* betamax-mean (mean xs))))

(define (maxll-variance n rss)
  (* (/ 1 n) rss))

