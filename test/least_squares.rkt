#lang racket

(require rackunit
         rackunit/text-ui
         csv-reading
         math/matrix
         math/array
         "../src/least_squares.rkt")

;FUNCTIONS
(define (extract-elements r)
  (map (λ (e) (if (eq? (string->number e) #f)
                  (string->symbol e)
                  (string->number e))) r))

(define (transpose-list nl)
  (apply map list nl))

(define (list-of-1 length) (build-list length (λ (_) 1)))

(define (list-of-0 length) (build-list length (λ (_) 0)))

(define (prepend-col-of-1 l)
  (let ([x (length l)])
    (list (list-of-1 x) l)))

(define (make-data-hash data)
  (let* ([t (transpose-list data)]
         [Y (list->matrix 1 (length (cdar t))
                        (cdar t))]
         [X (list*->matrix (prepend-col-of-1 (cdadr t)))])
    (hash (caar data) Y
          (cadar data) X)))

;MAIN
;prepare data
(define data (csv-map extract-elements
                      (make-csv-reader
                       (open-input-file "../resource/data.csv")
                       '((separator-chars #\,)
                         (strip-leading-whitespace? . #t)
                         (strip-trailing-whitespace? . #t)))))
(define data-hash (make-data-hash data))

(define bh
     (betahat (hash-ref data-hash 'x) (hash-ref data-hash 'y)))

(define fits (fit-ys (intercept bh)
                     (coefs bh)
                    (array-flatten
                   (array-slice-ref
                    (hash-ref data-hash 'x)
                    (list (:: 1 #f 1) (::))))))

(define rss (residual-sum-of-squares
                  fits
                  (array-flatten
                   (array-slice-ref
                    (hash-ref data-hash 'x)
                    (list (:: 1 #f 1) (::))))))

(define df (degrees-of-freedom (array-size (hash-ref data-hash 'y))
                               (matrix-num-rows (hash-ref data-hash 'x))))

(define sigma (sd-beta rss df))

;test
(define tests
  (test-suite
   "least squares test suite"

   (check-equal? bh (array #[#[7/2] #[7/5]]))
   (check-equal? fits (array #[49/10 63/10 77/10 91/10]))
   (check-equal? rss 409/5)
   (check-equal? df 2)
   (check-equal? sigma 409/10)
   ))
   
   (plot-least-squares (intercept bh)
                       (coefs bh)
                       (array-slice-ref
                        (hash-ref data-hash 'x) (list (:: 1 2 1) (::)))
                       (hash-ref data-hash 'y))
(run-tests tests)

