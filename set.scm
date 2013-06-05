(library (set)
  (export
    empty
    make-set
    empty? elem?
    union difference
    set-map)
  (import
    (rnrs base)
    (rnrs lists))

  (define empty '())

  (define (make-set x . xs)
    (cons x xs))

  (define (empty? set)
    (null? set))

  (define (elem? x set)
    (member x set))

  (define (union a b)
   (append a (difference b a)))

  (define (difference a b)
    (remp (lambda (x) (elem? x b)) a))

  (define (set-map f set)
    (map f set)))
