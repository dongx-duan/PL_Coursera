
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
   (cond [(> low high) '() ]
            [#t ( cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (x) ( string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [ (< n 0) (error "list-nth-mod: negative number")]
        [ (null? xs) (error "list-nth-mod: empty list") ]
        [ #t (let* ([len (length xs)]
                   [i (remainder n len)])
               (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (cond [ (= 0 n) '()]
        [ #t (let* ([p (s)])
               (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))]))

(define funny-number-stream
  (letrec ([f (lambda(x) 
                (cons
                   (if (= 0 (remainder x 5)) (- 0 x) x)
                              (lambda() ( f (+ x 1)))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([calldan (lambda() (cons "dan.jpg" calldog))]
           [calldog (lambda() (cons "dog.jpg" calldan))])
    calldan))

(define (stream-add-zero s)
  (lambda() 
    (let ([p (s)] )
      ( cons (cons 0 (car p)) (stream-add-zero (cdr p))))))

(define (cycle-lists xs ys)
  (letrec ([f ( lambda (n) 
                ( cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                        (lambda() (f (+ 1 n)))) )])
    (lambda()  (f 0) )))

(define (vector-assoc v vec)
  (letrec ([n (vector-length vec)]
           [f (lambda(i)
                ( if (= i n) 
                     #f
                     (let* ([v1 (vector-ref vec i)] ) 
                       ( if (and (pair? v1) ( equal? v (car v1))) 
                               v1  
                               (f (+ 1 i))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]     
           [f (lambda(v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! cache pos new-ans)
                              (set! pos (remainder (+ 1 pos) n))
                              new-ans)
                            #f)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([upper e1]
           [f (lambda()
                  (let ([i e2])
                    (if (< i upper)
                        (f)
                        #t)))])
       (f))]))
       
       

       