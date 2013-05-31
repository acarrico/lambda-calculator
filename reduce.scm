(load "ast.scm")
(load "set.scm")
(load "rename.scm")

; Set of free variables of term
(define (fv term)
  (cond
    [(var? term) (make-set term)]
    [(abst? term) (difference
                    (fv (body term))
                    (make-set (param term)))]
    [(appl? term) (union
                    (fv (left term))
                    (fv (right term)))]))

; Capture-avoiding substitution of term N for free occurrences of variable x in
; term M
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
                              (new-var (union (fv M) (fv N))))])
                     (make-abst
                       z
                       (sub x (sub y (body M) z) N)))))]

    [(appl? M) (make-appl
                 (sub x (left M) N)
                 (sub x (right M) N))]))

; Whether or not term is a redex
(define (redex? term)
  (cond
    [(var? term) #f]
    [(abst? term) #f]
    [(appl? term) (abst? (left term))]))

; Immediate β-reduct of redex
(define (beta redex)
  (sub
    (param (left redex))
    (body (left redex))
    (right redex)))

; Normal form of term, if one exists; otherwise does not terminate
(define (reduce term)
  (define (lftmos-helper term)
    (cond
      [(var? term) #f]
      [(abst? term) (let ([body-result (lftmos-helper (body term))])
                      (if body-result
                        (make-abst
                          (param term)
                          body-result)
                        #f))]
      [(appl? term) (if (redex? term)
                      (beta term)
                      (let ([left-result (delay (lftmos-helper (left term)))]
                            [right-result (delay (lftmos-helper (right term)))])
                        (cond
                          [(force left-result) (make-appl
                                                 (force left-result)
                                                 (right term))]
                          [(force right-result) (make-appl
                                                  (left term)
                                                  (force right-result))]
                          [else #f])))]))
  (let ([result (lftmos-helper term)])
    (if result
      (reduce result)
      term)))
