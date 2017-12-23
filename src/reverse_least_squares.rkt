#lang racket

(require math/matrix
         math/array)

(provide reverse-betahat)

(define (reverse-betahat coefs y)
  ;reversion is done KNOWING Y, since we really are
  ;looking for constraints on X giving Y
  (let ([c (->row-matrix coefs)])
    (matrix* c (matrix-inverse (matrix-transpose y)))))