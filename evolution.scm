(use extras)

(define variable? symbol?)

(define (application? x)
  (and (list? x)
       (= (length x) 2)
       (lambda-term? (car x))
       (lambda-term? (cadr x))))

(define application-function car)

(define application-argument cadr)

(define (lambda? x)
  (and (list? x)
       (= 3 (length x))
       (eq? 'l (car x))
       (variable? (cadr x))
       (lambda-term? (caddr x))))

(define lambda-argument cadr)

(define lambda-body caddr)

(define (lambda-term? x)
  (or (variable? x)
      (application? x)
      (lambda? x)))

(define (make-counter n)
  (lambda ()
    (let ([res n])
      (set! n (add1 n))
      res)))

(define variable-counter (make-counter 0))

(define (make-variable)
  (string->symbol (string-append "v" (number->string (variable-counter)))))

(define (random-mutation)
  (random 4))

(define (mutate term type var-index bound-vars free-vars)
  (case type
    [(0) ; bound variable
     (list-ref bound-vars var-index)]
    [(1) ; lambda abstraction binding new variable
     `(l ,(make-var) ,term)]
    [(2) ; apply term to bound variable
     `(,term ,(list-ref bound-vars var-index))]
    [(3) ; apply bound variable to term
     `(,(list-ref bound-vars var-index) ,term)]
    [else
      (error "unkonwn mutation code")]))

(define (make-probability-predicate prob)
  (lambda ()
    (< (random 100) (* prob 100))))

(define (terms term)
  (let loop ([t term]
             [count 1])
    (cond
      [(variable? t)
       count]
      [(lambda? t)
       (loop (lambda-body t) (add1 count))]
      [(application? t)
       (+ count
          (loop (application-function t) 1)
          (loop (application-argument t) 1))]
      [else
       (error "not a lambda term")])))


