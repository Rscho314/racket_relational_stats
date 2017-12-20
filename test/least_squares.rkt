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

(define rss (residual-sum-of-squares fits (hash-ref data-hash 'y)))

(define df (degrees-of-freedom (array-size (hash-ref data-hash 'y))
                               (matrix-num-rows (hash-ref data-hash 'x))))

(define residual-sd (sqrt (residual-variance rss df)))

(define rsq (r-square rss (hash-ref data-hash 'y)))

(define se-coefs (map
                  (λ (n) (se-coef (hash-ref data-hash 'x) residual-sd n))
                  (range 0 (array-size bh))))

(define ci (map (λ(c se) (confidence-interval c se 0.05)) (array->list bh) se-coefs))

;test
(define tests
  (test-suite
   "least squares test suite"

   (check-equal? bh (array #[#[7/2] #[7/5]]))
   (check-equal? fits (array #[49/10 63/10 77/10 91/10]))
   (check-equal? rss 21/5)
   (check-equal? df 2)
   (check-= residual-sd 1.44 0.01)
   (check-equal? rsq 7/10)
   (map (λ (x y) (check-= x y 0.01)) se-coefs '(1.77 0.64))
   (map (λ (x y) (check-= x y 0.01)) (flatten ci) '(0.021 6.97 0.12 2.67))
   ))
   
   (plot-least-squares (intercept bh)
                       (coefs bh)
                       (array-slice-ref
                        (hash-ref data-hash 'x) (list (:: 1 2 1) (::)))
                       (hash-ref data-hash 'y))
(run-tests tests)

