#lang racket

(require csv-reading)
(require rackunit "../src/one_sample_ttest.rkt")

(define (extract-element r)
  (string->number (first r)))

(define pop (csv-map extract-element (make-csv-reader
                        (open-input-file "../resource/sepal_length.csv")
                        '((separator-chars            #\,)
                          (strip-leading-whitespace?  . #t)
                          (strip-trailing-whitespace? . #t)))))

