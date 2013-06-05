; DISCLAIMER
;
; This parser works, but it was written in a time crunch and it needs to be
; completely refactored (or rewritten). It is really terrible Scheme code.

(library (parse)
  (export parse)
  (import
    (rnrs base)
    (rnrs r5rs)
    (rnrs lists)
    (fail)
    (ast))

  ; State-passing struct

  (define (make-state st-tree st-toks)
    (cons st-tree st-toks))

  (define (st-tree state)
    (car state))

  (define (st-toks state)
    (cdr state))

  ; Parser combinators

  (define-syntax alternate
    (syntax-rules ()
      [(_ p ...)
       (lambda (toks)
         (or (p toks) ...))]))

  ; Terminals

  (define (term sym)
    (lambda (toks)
      (if (and
            (not (null? toks))
            (equal? (car toks) sym))
        (make-state
          sym
          (cdr toks))
        #f)))

  (define (var toks)
    (if (and
          (not (null? toks))
          (string? (car toks)))
      (make-state
        (make-var (car toks))
        (cdr toks))
      #f))

  ; Productions

  (define (parens toks)
    (let* ([r1 (delay ((term 'oparen) toks))]
           [r2 (delay (exprs (st-toks (force r1))))]
           [r3 (delay ((term 'cparen) (st-toks (force r2))))])
      (if (and
            (force r1)
            (force r2)
            (force r3))
        (make-state
          (st-tree (force r2))
          (st-toks (force r3)))
        #f)))

  (define (params-helper toks)
    (let* ([r1 (delay (var toks))]
           [r2 (delay (params-helper (st-toks (force r1))))])
      (cond
        [(and (force r1) (force r2))
         (make-state
           (cons (st-tree (force r1)) (st-tree (force r2)))
           (st-toks (force r2)))]
        [(force r1)
         (make-state
           (cons (st-tree (force r1)) '())
           (st-toks (force r1)))]
        [else #f])))

  (define (lam toks)
    (let* ([r1 (delay ((term 'lam) toks))]
           [r2 (delay (params-helper (st-toks (force r1))))]
           [r3 (delay ((term 'dot) (st-toks (force r2))))]
           [r4 (delay (exprs (st-toks (force r3))))])
      (cond
        [(and (force r1) (force r2) (force r3) (force r4))
         (make-state
           (fold-right make-abst (st-tree (force r4)) (st-tree (force r2)))
           (st-toks (force r4)))]
        [else #f])))

  (define expr
    (alternate
      lam
      parens
      var))

  (define (exprs toks)

    (define (exprs-helper toks)
      (let* ([r1 (delay (expr toks))]
             [r2 (delay (exprs-helper (st-toks (force r1))))])
        (cond
          [(and (force r1) (force r2))
           (make-state
             (cons (st-tree (force r1)) (st-tree (force r2)))
             (st-toks (force r2)))]
          [(force r1)
           (make-state
             (cons (st-tree (force r1)) '())
             (st-toks (force r1)))]
          [else #f])))

      (let ([r (exprs-helper toks)])
        (if r
          (make-state
            (fold-left make-appl (car (st-tree r)) (cdr (st-tree r)))
            (st-toks r))
          #f)))

  ; Top-level parser

  (define (parse toks)
    (let ([r (exprs toks)])
      (if (and
            r
            (null? (st-toks r)))
        (st-tree r)
        (fail "Parse error.")))))
