#!r6rs

(library (lambda-calculator print)
  (export print)
  (import
    (rnrs base)
    (lambda-calculator ast))

  (define (parenthesize str)
    (string-append "(" str ")"))

  (define (print-var var)
    (name var))

  (define (print-abst abst)
    (define (param-helper expr)
      (string-append
        (print-var (param expr))
        " "
        (if (abst? (body expr))
          (param-helper (body expr))
          (string-append ". " (print (body expr))))))
    (string-append "λ " (param-helper abst)))

  (define (print-appl appl)
    (define (paren-helper lftmost? rtmost? expr)
      (cond
        [(var? expr) (print-var expr)]
        [(abst? expr) (if rtmost?
                        (print-abst expr)
                        (parenthesize (print-abst expr)))]
        [(appl? expr) (let ([result (string-append
                                      (paren-helper lftmost? #f (left expr))
                                      " "
                                      (paren-helper #f rtmost? (right expr)))])
                        (if lftmost?
                          result
                          (parenthesize result)))]))
    (paren-helper #t #t appl))

  (define (print expr)
    (cond
      [(var? expr) (print-var expr)]
      [(abst? expr) (print-abst expr)]
      [(appl? expr) (print-appl expr)])))
