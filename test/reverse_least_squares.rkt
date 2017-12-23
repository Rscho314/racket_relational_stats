#lang racket

(require rackunit
         rackunit/text-ui
         math/matrix
         "../src/data_importer.rkt"
         "../src/reverse_least_squares.rkt")

(define data (make-data-hash "../resource/data.csv"))
(define Y (hash-ref data 'y))
(define X (hash-ref data 'x))

(define tests
  (test-suite
   "reverse least squares test suite"

   (check-equal? (reverse-betahat '(3.5 1.44) Y) (row-matrix [1 2 3]))
   ))
(run-tests tests)