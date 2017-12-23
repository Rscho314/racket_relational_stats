#lang racket

(require csv-reading
         math/matrix)

(provide make-data-hash)

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

(define (csv-reader path) (csv-map extract-elements
                      (make-csv-reader
                       (open-input-file path)
                       '((separator-chars #\,)
                         (strip-leading-whitespace? . #t)
                         (strip-trailing-whitespace? . #t)))))

(define (make-data-hash path)
  (let* ([data (csv-reader path)]
         [t (transpose-list data)]
         [Y (list->matrix 1 (length (cdar t))
                          (cdar t))]
         [X (list*->matrix (prepend-col-of-1 (cdadr t)))])
    (hash (caar data) Y
          (cadar data) X)))