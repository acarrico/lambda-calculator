; Variable

(define (make-var name)
  (cons 'var name))

(define (var? term)
  (eqv? (car term) 'var))

(define (name var)
  (cdr var))

; Abstraction

(define (make-abst param body)
  (cons 'abst (cons param body)))

(define (abst? term)
  (eqv? (car term) 'abst))

(define (param abst)
  (cadr abst))

(define (body abst)
  (cddr abst))

; Application

(define (make-appl left right)
  (cons 'appl (cons left right)))

(define (appl? term)
  (eqv? (car term) 'appl))

(define (left appl)
  (cadr appl))

(define (right appl)
  (cddr appl))
