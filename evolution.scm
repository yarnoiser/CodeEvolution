(use extras (srfi 1 69))

(define variable? symbol?)

(define (application? x)
  (and (list? x)
       (= (length x) 2)))

(define application-function car)

(define application-argument cadr)

(define (lambda? x)
  (and (list? x)
       (= 3 (length x))
       (eq? 'l (car x))
       (variable? (cadr x))))

(define lambda-argument cadr)

(define lambda-body caddr)

(define (lambda-term? x)
  (or (variable? x)
      (application? x)
      (lambda? x)))

(define (alpha-equal? term1 term2)
  (equal? (rename term1) (rename term2)))

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

(define (beta-reduce term #!optional (application-limit 10000))
  (define applications 0)
  (define (beta-reduce-step term)
    (cond
      [(>= applications application-limit)
       #f]
      [(variable? term)
       term]
      [(lambda? term)
       `(l ,(lambda-argument term) ,(beta-reduce-step (lambda-body term)))]
      [(application? term)
       (set! applications (add1 applications))
       (if (not (lambda? (application-function term)))
         `(,(beta-reduce-step (application-function term)) ,(beta-reduce-step (application-argument term)))
         (beta-reduce-step (substitute (lambda-body (application-function term)) (lambda-argument (application-function term)) (application-argument term))))]
      [else
       (error "Not a lambda term")]))
  (define (reduce term)
    (let ([reduced (beta-reduce-step term)])
      (if (equal? reduced term)
        term
       (reduce reduced))))
  (reduce (rename term)))

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

(define (number->church number)
  `(l f (l x ,(let loop ([n number])
                (if (= n 0)
                  'x
                  `(f ,(loop (sub1 n))))))))

(define (church-numeral? term)
  (cond
    [(not (lambda? term))
     #f]
    [(not (lambda? (lambda-body term)))
     #f]
    [else
     (let ([f1 (lambda-argument term)]
           [f2 (lambda-argument (lambda-body term))])
       (let loop ([t (lambda-body (lambda-body term))])
         (cond
           [(and (variable? t) (eq? t f2))
            #t]
           [(and (application? t) (eq? (application-function t) f1))
            (loop (application-argument t))]
           [else
            #f])))]))

(define (church->number term)
  (let ([body (lambda-body (lambda-body term))])
    (let loop ([t body]
               [n 0])
      (if (variable? t)
        n
        (loop (application-argument t) (add1 n))))))

(define (boolean->church bool)
  (if bool
    '(l a (l b a))
    '(l a (l b b))))

(define (church-boolean? term)
  (cond
    [(not (lambda? term))
     #f]
    [(not (lambda? (lambda-body term)))
     #f]
    [(not (variable? (lambda-body (lambda-body term))))
     #f]
    [else
      (let ([a (lambda-argument term)]
            [b (lambda-argument (lambda-body term))]
            [res (lambda-body (lambda-body term))])
        (or (eq? res b) (eq? res b)))]))

(define (church->boolean term)
  (let ([a (lambda-argument term)]
        [b (lambda-argument (lambda-body term))]
        [res (lambda-body (lambda-body term))])
    (if (eq? res a)
      #t
      #f)))

(define church-null '(l p ((p (l a (l b a))) (l a (l b a)))))

(define (pair->church pair)
  (cond
    [(null? pair)
     church-null]
    [(not (pair? pair))
     pair]
    [else
    `(l p ((p ,(car pair)) ,(pair->church (cdr pair))))]))

(define (church-pair? term)
  (cond
    [(not (lambda? term))
      #f]
    [(not (application? (lambda-body term)))
     #f]
    [(not (application? (application-function (lambda-body term))))
     #f]
    [(not (variable? (application-function (application-function (lambda-body term)))))
     #f]
    [else
      (eq? (lambda-argument term) (application-function (application-function (lambda-body term))))]))

(define (church->pair term)
  (let ([a (application-argument (application-function (lambda-body term)))]
        [b (application-argument (lambda-body term))])
    (cond
      [(alpha-equal? church-null term)
       '()]
      [(church-pair? b)
       `(,a . ,(church->pair b))]
      [else
       `(,a . ,b)])))

(define (number-list->church lst)
  (pair->church (map number->church lst)))

(define (church->number-list term)
  (map church->number (church->pair term)))

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

(define (sort-format? term)
  (if (not (church-pair? term))
    #f
    (let ([lst (church->pair term)])
      (every church-numeral? lst))))

