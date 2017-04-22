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


