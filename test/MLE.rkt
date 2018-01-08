#lang racket

(require rackunit
         rackunit/text-ui
         math/matrix
         math/array
         "../src/MLE.rkt"
         "../src/data_importer.rkt")

(define data (make-data-hash "../resource/sepal.csv"))

;refactor data-hash structure to avoid this bad hack
(define Sepal.Width (vector->array (matrix->vector (matrix-row (hash-ref data 'Sepal.Width) 1))))

(define bmax (maxll-beta Sepal.Width
                         (hash-ref data 'Sepal.Length)))
(define intercept-max (maxll-intercept Sepal.Width
                                       (hash-ref data 'Sepal.Length)
                                       bmax))
(define variance-max (maxll-variance (array-size (hash-ref data 'Sepal.Length))
                                     (residual-sum-of-squares
                                      (fit-ys intercept-max (list bmax) Sepal.Width)
                                      (hash-ref data 'Sepal.Length))))
(define ptll (negative-log-likelihood Sepal.Width
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

