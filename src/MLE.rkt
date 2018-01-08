#lang racket
;TODO: REFACTOR USING STATISTICS OBJECTS
(require math/array
         math/statistics)

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
  (array-map (regression-equation intercept coefs) xs))

(define (residual-sum-of-squares fit data)
  (array-all-sum (array-sqr (array- fit data))))

(define (negative-log-likelihood xs ys intercept coefs variance)
  (letrec ([fits (fit-ys intercept coefs xs)]
           [rss (residual-sum-of-squares fits ys)]
           [n (array-size ys)])
    (* -1 (- (* (/ n -2) (log (* 2 pi)))
       (* (/ n 2) (log variance))
       (*(/ 1 (* 2 variance)) rss)))
    ))

(define (maxll-beta xs ys)
  (letrec ([n (array-size ys)]
        [x-mean (mean (array->list xs))]
        [y-mean (mean (array->list ys))]
        [x-normalized (array-map (λ(e) (- e x-mean)) xs)]
        [y-normalized (array-map (λ(e) (- e y-mean)) ys)])
    (/ (array-all-sum (array* x-normalized y-normalized))
       (array-all-sum (array-sqr x-normalized)))
    ))

(define (maxll-intercept xs ys betamax-mean)
  (let ([x-mean (mean (array->list xs))]
        [y-mean (mean (array->list ys))])
  (- y-mean (* betamax-mean x-mean))))

(define (maxll-variance n rss)
  (* (/ 1 n) rss))

