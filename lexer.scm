(define (terminal? char)
  (case char
    [#\λ 'lam]
    [#\. 'dot]
    [#\( 'oparen]
    [#\) 'cparen]
    [#\[ 'osquare]
    [#\] 'csquare]
    [else #f]))

(define (charlst->toklst clst)
  (define (name-helper acc clst)
    (if (or
          (null? clst)
          (char-whitespace? (car clst))
          (terminal? (car clst)))
      (cons
        (cons 'name acc)
        (charlst->toklst clst))
      (name-helper (string-append acc (string (car clst))) (cdr clst))))
  (cond
    [(null? clst) '()]
    [(char-whitespace? (car clst)) (charlst->toklst (cdr clst))]
    [(terminal? (car clst)) (cons
                              (terminal? (car clst))
                              (charlst->toklst (cdr clst)))]
    [else (name-helper "" clst)]))

(define (tokenize str)
  (charlst->toklst (string->list str)))
