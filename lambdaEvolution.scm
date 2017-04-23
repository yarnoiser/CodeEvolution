(use extras)

(define variable? symbol?)

(define (application? x)
  (and (list? x)
       (= length x 2)))

(define application-function car)

(define application-argument cadr)

(define (lambda? x)
  (and (pair? x)
       (not (list? x))
       (eq? 'l (car x))
       (symbol? (cadr x))
       (not (null? (cddr x)))))

(define lambda-argument cadr)

(define lambda-body cddr)

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
  (random 7))

(define (mutate term type var-index bound-vars free-vars)
  (case type
    [(0) ; new variable
     (make-variable)]
    [(1) ; free variable
     (list-ref free-vars var-index)]
    [(2) ; bound variable
     (list-ref bound-vars var-index)]
    [(3) ; lambda abstraction new variable
     `(l ,(make-var) . ,term)]
    [(4) ; lambda abstraction free variable
     `(l ,(list-ref free-vars var-index) . ,term)]
    [(5) ; appy term to new variable
     `(,term ,(make-variable))]
    [(6) ; apply term to free variable
     `(,term ,(list-ref free-vars var-index))]
    [(7) ; apply term to bound variable
     `(,term ,(list-ref bound-vars var-index))]
    [(8) ; apply new variable to term
     `(,(make-variable) ,term)]
    [(9) ; apply free variable to term
     `(,(list-ref free-vars var-index) ,term)]
    [(10) ; apply bound variable to term
     `(,(list-ref bound-vars var-index) ,term)]))

