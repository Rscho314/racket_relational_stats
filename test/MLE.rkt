#lang racket

(require rackunit
         rackunit/text-ui
         math/flonum
         "../src/MLE.rkt"
         "../src/data_importer.rkt")

(define data (make-data-hash "../resource/sepal.csv"))

(define bmax (maxll-beta (hash-ref data 'Sepal.Width)
                         (hash-ref data 'Sepal.Length)))
(define intercept-max (maxll-intercept (hash-ref data 'Sepal.Width)
                                       (hash-ref data 'Sepal.Length)
                                       bmax))
(define variance-max (maxll-variance (flvector-length (hash-ref data 'Sepal.Length))
                                     (residual-sum-of-squares
                                      (fit-ys intercept-max (list bmax) (hash-ref data 'Sepal.Width))
                                      (hash-ref data 'Sepal.Length))))
(define ptll (negative-log-likelihood (hash-ref data 'Sepal.Width)
                                      (hash-ref data 'Sepal.Length)
                                      intercept-max
                                      (list bmax)
                                      variance-max))

;test
(define tests
  (test-suite
   "MLE test suite"


   ))
(run-tests tests)

