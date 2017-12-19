#lang rosette

;(define-symbolic* a b c m x y z n real?)
;(current-bitwidth #f)
;
;(define (my-sum a b c)
;  (solve (begin (assert (= c (+ a b)))
;                (assert (< a 5))
;                (assert (> a 0))
;                (assert (< b 5))
;                (assert (> b 0)))))
;
;(evaluate c (my-sum 3 4 c))
;(evaluate b (my-sum 3 b 7))
;(evaluate (list a b) (my-sum a b 6.9))
;
;(define l (take (list x y z) n))
;(define (my-mean l m)
;  (solve (begin
;           (assert (> (length l) 2))
;           (assert (eq? (null? l) #f))
;           (assert (= (length l) (length (remove-duplicates l))))
;           (assert (= (/ (foldl + 0 l) (length l)) m)))))
;
;(evaluate m (my-mean (list 1 2 3) m))
;(evaluate l (my-mean l 4.5))

; one sample t-test
;(provide t-stat)

(define (square-rec n)
  (let ([nu (numerator n)]
        [de (denominator n)])
  (define (sq o i b)
    (if (> i 0) (sq o (- i 1) (+ b o))
        b))
  (/ (sq nu nu 0) (sq de de 0))))

(define (sqrc n)
  (define (rec i)
    (let ([ii (floor (/ i 2))])
      (cond ((odd? i) (+ 1 (+ (* 4 (rec ii)) (* 4 ii))))
            ((zero? i) 0)
            ((even? i) (* 4 (rec ii))))))
  (/ (rec (numerator n)) (rec (denominator n))))


(current-bitwidth #f)

(define-symbolic* a b c d i m v r mu s n real?)

(define l (take (list a b c) d))
(define (mean li me)
  (solve (begin (assert (eq? (null? li) #f))
                (assert (= me (/ (foldl + 0 li) (length li)))))))

(define (variance li me va)
  (solve (begin
           (assert (eq? (null? li) #f))
           (assert (= va (/ (foldl + 0 (map (λ (e) (sqrc (- e me))) li)) (- (length li) 1)))))))

(define pop (list 0.5 0.3 0.2))
(define v1 (evaluate v (variance pop (evaluate m (mean pop m)) v)))
;(evaluate l (variance l m1 v1))

;(define (sd var)

;(define (t-stat m mu s n t)
;  (solve (begin
;           (assert (= t (/ (- m mu) (/ s (expt n 1/2))))))))