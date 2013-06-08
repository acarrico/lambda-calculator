#!r6rs

(library (reduce)
  (export
    reduce-norm
    reduce-applic)
  (import
    (rnrs base)
    (rnrs r5rs)
    (lambda-calculator ast)
    (lambda-calculator set)
    (lambda-calculator rename))

  ; Set of free variables of expr
  (define (fv expr)
    (cond
      [(var? expr) (make-set expr)]
      [(abst? expr) (difference
                      (fv (body expr))
                      (make-set (param expr)))]
      [(appl? expr) (union
                      (fv (left expr))
                      (fv (right expr)))]))

  ; Capture-avoiding substitution of expression N for free occurrences of
  ; variable x in expression M
  (define (sub x M N)
    (cond
      [(var? M) (if (equal? M x)
                  N
                  M)]
      [(abst? M) (let ([y (param M)])
                   (if (equal? y x)
                     M
                     (let ([z (if (or
                                    (not (elem? x (fv M)))
                                    (not (elem? y (fv N))))
                                y
                                (make-var
                                  (new-name (union (fv M) (fv N)))))])
                       (make-abst
                         z
                         (sub x (sub y (body M) z) N)))))]

      [(appl? M) (make-appl
                   (sub x (left M) N)
                   (sub x (right M) N))]))

  ; Whether or not expr is a redex
  (define (redex? expr)
    (cond
      [(var? expr) #f]
      [(abst? expr) #f]
      [(appl? expr) (abst? (left expr))]))

  ; β-reduct of redex
  (define (beta redex)
    (sub
      (param (left redex))
      (body (left redex))
      (right redex)))

  ; Normal form of expr, if one exists; otherwise does not terminate
  (define (reduce-norm expr)
    (define (lftmos-helper expr)
      (cond
        [(var? expr) #f]
        [(abst? expr) (let ([body-result (lftmos-helper (body expr))])
                        (if body-result
                          (make-abst
                            (param expr)
                            body-result)
                          #f))]
        [(appl? expr) (if (redex? expr)
                        (beta expr)
                        (let ([left-result (delay (lftmos-helper (left expr)))]
                              [right-result (delay (lftmos-helper (right expr)))])
                          (cond
                            [(force left-result) (make-appl
                                                   (force left-result)
                                                   (right expr))]
                            [(force right-result) (make-appl
                                                    (left expr)
                                                    (force right-result))]
                            [else #f])))]))
    (let ([result (lftmos-helper expr)])
      (if result
        (reduce-norm result)
        expr)))

  (define (reduce-applic expr)
    (define (rtmos-helper expr)
      (cond
        [(var? expr) #f]
        [(abst? expr) (let ([body-result (rtmos-helper (body expr))])
                        (if body-result
                          (make-abst
                            (param expr)
                            body-result)
                          #f))]
        [(appl? expr) (if (redex? expr)
                        (beta expr)
                        (let ([left-result (delay (rtmos-helper (left expr)))]
                              [right-result (delay (rtmos-helper (right
                                                                    expr)))])
                          (cond
                            [(force right-result) (make-appl
                                                    (left expr)
                                                    (force right-result))]
                            [(force left-result) (make-appl
                                                   (force left-result)
                                                   (right expr))]
                            [else #f])))]))
    (let ([result (rtmos-helper expr)])
      (if result
        (reduce-applic result)
        expr))))
