;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist _lst)
  (letrec ([f (λ(lst) (cond [(null? lst) (aunit)]
                         [#t (apair (car lst) (f (cdr lst)))]))])
    (f _lst)))

(define (mupllist->racketlist _lst)
  (letrec ([f (λ(lst) (cond [(aunit? lst) null]
                            [#t (cons (apair-e1 lst) (f (apair-e2 lst)))]))])
    (f _lst)))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL conditional applied to non-integers")))]
        [(mlet? e)
         (let* ([v1 (eval-under-env (mlet-e e) env)]
                [v2 (mlet-var e)]
                [new-env (cons (cons v2 v1) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd applied to non-pair")))]
        [(fun? e)
         (closure env e)]
        [(closure? e) e]
        [(call? e) 
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([c1 (closure-fun v1)]
                     [c2 (closure-env v1)]
                     [cn (cons (fun-nameopt c1) v1)]
                     [cf (cons (fun-formal c1) v2)])
                 (eval-under-env 
                  (fun-body c1)
                  (if (eq? (car cn) #f)
                      (cons cf c2)
                      (cons cf (cons cn c2)))))
               (error "MUPL call applied to non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (call (fun #f "" (ifgreater (isaunit e1) (int 0) e2 e3)) (aunit)))

(define (mlet* lstlst e2)
  (let ([f (λ(pr exp)
             (let ([mletv (car pr)]
                   [mletexp (cdr pr)])
               (mlet mletv mletexp exp)))])
    (foldr f e2 lstlst)))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2
    (ifgreater (var "_x") (var "_y") e4
               (ifgreater (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define mupl-map
  (fun "outer" "mapf"
    (fun "_map" "lst"
       (ifgreater (isaunit (var "lst")) (int 0) (var "lst") ; done recursing
                  (apair (call (var "mapf") (fst (var "lst"))) (call (var "_map") (snd (var "lst"))))))
    ))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
