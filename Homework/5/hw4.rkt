
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; 1
(define (sequence low high stride)
  (letrec ([ build (λ (lst) 
                  (if (> (+ (car lst) stride) high)
                      lst
                      (build (cons (+ (car lst) stride) lst))))])
    (if (>= low high)
        (list)
        (reverse (build (list low))))))


;; 2
(define (string-append-map xs suffix)
  (map (λ (s) (string-append s suffix)) xs))


;; 3
(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (empty? xs) (error "list-nth-mod: empty list")
      (let ([i (remainder n (length xs))])
        (car (list-tail xs i))))))


;; 4
(define (stream-for-n-steps s n)
  (letrec ([gen (λ (lst count s)
               (if (= count 0)
                   lst
                   (gen (cons (car (s)) lst) (- count 1) (cdr (s)) )))])
    (reverse (gen (list) n s))))


;; 5
(define funny-number-stream
  (letrec ([s (λ (x)
                (if (= 0 (remainder x 5))
                (cons (* -1 x) (λ () (s (+ x 1))))
                (cons x (λ () (s (+ x 1))))))
           ])
           (λ () (s 1))))


;; 6
(define dan-then-dog
  (letrec ([f (λ () (cons "dan.jpg" (λ () (cons "dog.jpg" (λ () (f))))))])
    f))


;; 7
(define (stream-add-zero s)
  (letrec ([f (λ(s) (cons (cons 0 (car (s))) (λ() (f (cdr (s))))))])
    (λ() (f s)))) 


;; 8
(define (cycle-lists _xs _ys)
  (letrec ([f (λ(xs ys) (cons
                         (cons (if (null? xs) (car _xs) (car xs))
                         (if (null? ys) (car _ys) (car ys)))
                         (λ() (f (if (null? xs) _xs (list-tail xs 1)) (if (null? ys) _ys (list-tail ys 1))))))])
    (λ () (f _xs _ys))))


;; 9
(define (vector-assoc v vec)
  (letrec ([iterate (λ(n) (cond [(= n (vector-length vec)) #f] ;end of vector
                                [(not (pair? (vector-ref vec n))) (iterate (+ n 1))] ;current element not a pair
                                [(equal? v (car (vector-ref vec n))) (vector-ref vec n)] ;found it!
                                [#t (iterate (+ n 1))] ;not found, next element
                           ))])
    (iterate 0)))
  

;; 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [c-pointer 0])
    
    (λ(v) (cond [(vector-assoc v cache) (vector-assoc v cache) ]
                [#t (let ([val (assoc v xs)])
                      (if val ((λ() (vector-set! cache c-pointer val)
                              (if (= c-pointer (- (vector-length cache) 1))
                                  (set! c-pointer 0)
                                  (+ c-pointer 1))
                              val)) #f))]))))