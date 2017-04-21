(use extras)

(define (make-prob-pred prob)
  (lambda ()
    (let ([percent (inexact->exact (* 100 prob))])
      (if (< (random 100) percent)
        #t
        #f))))

(define (random-mutation)
  (case (random 3)
    [(0) 'ins]
    [(1) 'del]
    [(2) 'alt]
    [else (error "unkown mutation type")]))

(define (mutation possible-mutations)
  (cond
    [(not (or (list? possible-mutations) (vector possible-mutations)))
     (error "no possible mutations given")]
    [(list? possible-mutations)
     (mutation (list->vector possible-mutations))]
    [else
     (vector-ref possible-mutations (random (vector-length possible-mutations)))]))

(define (mutate lst probability mutation-type #!optional (possible-mutations #f))
  (mutate-list lst (make-prob-pred probability) mutation-type possible-mutations))

(define (mutate-list lst mutate? mutation-type possible-mutations)
  (let ([type (if (eq? mutation-type 'ran)
               (random-mutation)
                mutation-type)]
        [recur (lambda (l)
                 (mutate-list l mutate? mutation-type possible-mutations))])
    (cond
      [(not (list? lst))
       lst]
      [(null? lst)
       lst]
      [(not (mutate?))
       (cons (recur (car lst)) (recur (cdr lst)))]
      [(eq? type 'del)
       (recur (cdr lst))]
      [(eq? type 'alt)
       (cons (mutation possible-mutations) (recur (cdr lst)))]
      [(eq? type 'ins)
       (cons (mutation possible-mutations) (cons (recur (car lst)) (recur (cdr lst))))]
      [else
       (error "unkown mutation type")])))

(define test (mutate '(1 2 3 4 5) 1/5 'ran '(#f)))


(define (make-counter n)
  (lambda ()
    (let ([old-n n])
      (set! n (add1 n))
      old-n)))

(define symbol-counter (make-counter 0))

(define (new-var)
  (string->symbol (string-append "s" (number->string (symbol-counter)))))

(define var? symbol?)

(define (lambda? x)
  (and (list? x)
       (= (length x) 3)
       (eq? (car x) 'lambda)
       (list? (cadr x))))

(define lambda-var caadr)

(define lambda-body caddr)

(define (app? x)
  (and (list? x)
       (= (length x) 2)))

(define app-called car)

(define app-input cadr)

(define (lambda-term? x)
  (or (lambda? x)
      (app? x)
      (var? x)))

(define (random-element lst)
  (list-ref (random (length lst))))

(define (add-lambda f)
  `(lambda (,(new-var))
     ,f))

(define (add-app symb expr)
  `(,symb ,expr))


