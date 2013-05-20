(define empty '())

(define (make-set x . xs)
  (cons x xs))

(define (empty? set)
  (null? set))

(define (elem? x set)
  (if (member x set)
    #t
    #f))

(define (union a b)
 (append a (difference b a)))

(define (difference a b)
  (remp (lambda (x) (elem? x b)) a))

(define (set-map f set)
  (map f set))
