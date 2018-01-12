#lang racket

(require csv-reading
         math/flonum)

(provide make-data-hash)

(define (extract-elements r)
  (map (λ (e) (if (eq? (string->number e) #f)
                  (string->symbol e)
                  (string->number e))) r))

(define (transpose-list nl)
  (apply map list nl))

(define (vector-of-1 length) (build-flvector length (λ (_) 1)))

(define (vector-of-0 length) (build-flvector length (λ (_) 0)))

(define (prepend-col-of-1 v)
  (let ([x (vector-length v)])
    (vector-immutable (vector-of-1 x) v)))

(define (csv-reader path) (csv-map extract-elements
                      (make-csv-reader
                       (open-input-file path)
                       '((separator-chars #\,)
                         (strip-leading-whitespace? . #t)
                         (strip-trailing-whitespace? . #t)))))

(define (make-data-hash path)
  (letrec ([data-raw (csv-reader path)]
           [data-vectors (map list->flvector (transpose-list (cdr data-raw)))]
           [names (car data-raw)])
    
    (make-immutable-hash (map (λ(a b) (cons a b)) names data-vectors))
    ))