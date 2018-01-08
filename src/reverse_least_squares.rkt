#lang racket

(require math/distributions)

(provide reverse-p-value)

(define (reverse-p-value p-value [sides 2])
  (let ([abs (inv-cdf (normal-dist) (/ p-value sides))])
    (list (* -1 abs) abs)))

(define (t-stat coef se-coef [hypothesis 0])
  (/ (- coef hypothesis) se-coef))
(define (reverse-t-stat t-stat [hypothesis 0])
  ;we do not know coef every time
  1)

(define (confidence-interval coef se-coef alpha [sides 2])
  (map (λ(b) (+ coef (* b se-coef)))
         (map (λ(x) (inv-cdf (normal-dist) x))
              (list (/ alpha sides) (- 1 (/ alpha sides))))))
(define (reverse-confidence-interval ci))