(load "ast.scm")

(define (paren str)
  (string-append "(" str ")"))

(define (var->string var)
  (name var))

(define (abst->string abst)
  (define (param-helper term)
    (string-append
      (var->string (param term))
      " "
      (if (abst? (body term))
        (param-helper (body term))
        (string-append ". " (term->string (body term))))))
  (string-append "λ " (param-helper abst)))

(define (appl->string appl)
  (define (helper lftmost? rtmost? term)
    (cond
      [(var? term) (var->string term)]
      [(abst? term) (if rtmost?
                      (abst->string term)
                      (paren (abst->string term)))]
      [(appl? term) (let ([result (string-append
                                    (helper lftmost? #f (left term))
                                    " "
                                    (helper #f rtmost? (right term)))])
                      (if lftmost?
                        result
                        (paren result)))]))
  (helper #t #t appl))

(define (term->string term)
  (cond
    [(var? term) (var->string term)]
    [(abst? term) (abst->string term)]
    [(appl? term) (appl->string term)]))
