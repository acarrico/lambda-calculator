#!r6rs

(library (ast)
  (export
    make-var var? name
    make-abst abst? param body
    make-appl appl? left right)
  (import (rnrs base))

  ; Variable

  (define (make-var name)
    (cons 'var name))

  (define (var? expr)
    (eqv? (car expr) 'var))

  (define (name var)
    (cdr var))

  ; Abstraction

  (define (make-abst param body)
    (cons 'abst (cons param body)))

  (define (abst? expr)
    (eqv? (car expr) 'abst))

  (define (param abst)
    (cadr abst))

  (define (body abst)
    (cddr abst))

  ; Application

  (define (make-appl left right)
    (cons 'appl (cons left right)))

  (define (appl? expr)
    (eqv? (car expr) 'appl))

  (define (left appl)
    (cadr appl))

  (define (right appl)
    (cddr appl)))
