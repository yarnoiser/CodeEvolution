(use extras srfi-69)

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

(define (make-variable-maker count)
  (lambda ()
    (let ([v (string->symbol (string-append "v" (number->string count)))])
      (set! count (add1 count))
      v)))

(define make-variable (make-variable-maker 0))

(define (substitute term variable value)
  (cond
    [(variable? term)
     (if (eq? term variable)
       value
       term)]
    [(lambda? term)
     `(l ,(lambda-argument term) ,(substitute (lambda-body term) variable value))]
    [(application? term)
     `(,(substitute (application-function term) variable value) ,(substitute (application-argument term) variable value))]))

(define (rename-single term variable new-variable)
  (cond
    [(variable? term)
     (if (eq? term variable)
       new-variable
       term)]
    [(lambda? term)
     `(l ,(rename-single (lambda-argument term) variable new-variable)
         ,(rename-single (lambda-body term) variable new-variable))]
    [(application? term)
     `(,(rename-single (application-function term) variable new-variable)
       ,(rename-single (application-argument term) variable new-variable))]))

(define (rename term)
  (let ([make-variable (make-variable-maker 0)]
        [renamed (make-hash-table)])
    (let loop ([t term])
      (cond
        [(variable? t)
         (if (hash-table-exists? renamed t)
           t
           (let ([new-var (make-variable)])
             (hash-table-set! renamed new-var t)
             new-var))]
        [(lambda? t)
         (if (hash-table-exists? renamed (lambda-argument t))
           `(l ,(lambda-argument t) ,(loop (lambda-body t)))
            (let* ([new-var (make-variable)]
                   [result (rename-single t (lambda-argument t) new-var)])
               (hash-table-set! renamed new-var #t)
              `(l ,(lambda-argument result) ,(loop (lambda-body result)))))]
        [(application? t)
         `(,(loop (application-function t)) ,(loop (application-argument t)))]))))

(define (beta-reduce term)
  (cond
    [(variable? term)
     term]
    [(lambda? term)
     `(l ,(lambda-argument term) ,(beta-reduce (lambda-body term)))]
    [(application? term)
     (if (not (lambda? (application-function term)))
       `(,(beta-reduce (application-function term)) ,(beta-reduce (application-argument term)))
       (beta-reduce (substitute (lambda-body (application-function term)) (lambda-argument (application-function term)) (application-argument term))))]))

(define (num-terms term)
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

(define (random-mutation bound)
  (if (null? bound)
    0
    (random 4)))

(define (mutate term type var-index bound-vars)
  (case type
    [(0) ; lambda abstraction binding new variable
     `(l ,(make-variable) ,term)]
    [(1) ; bound variable
     (list-ref bound-vars var-index)]
    [(2) ; apply term to bound variable
     `(,term ,(list-ref bound-vars var-index))]
    [(3) ; apply bound variable to term
     `(,(list-ref bound-vars var-index) ,term)]
    [else
      (error "unkonwn mutation code")]))

(define (make-probability-predicate prob)
  (lambda ()
    (< (random 100) (* prob 100))))

(define (replicate prob term)
  (let ([mutate? (make-probability-predicate prob)])
    (let loop ([t term]
               [bound '()])
      (let ([result (if (mutate?)
                      (mutate t (random-mutation bound) (random (length bound)) bound)
                      t)])
        (cond
          [(variable? result)
           result]
          [(lambda? result)
           `(l ,(lambda-argument result) ,(loop (lambda-body result) (cons (lambda-argument result) bound)))]
          [(application? result)
           `(,(loop (application-function result) bound) ,(loop (application-argument result) bound))]
          [(error "not a lambda term")])))))

