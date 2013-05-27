(load "set.scm")

(define (next-char c)
  (case c
    [#\z #\a]
    [else (integer->char (+ (char->integer c) 1))]))

(define (next-string str)
  (case str
    ["" "a"]
    [else (let* ([rev-clst (reverse (string->list str))])
            (list->string (reverse
              (case (car rev-clst)
                [#\z (cons
                       #\a
                       rev-clst)]
                [else (cons
                        (next-char (car rev-clst))
                        (cdr rev-clst))]))))]))

(define (new-name taken)
  (let ([result (next-string "")])
    (if (not (elem? result taken))
      result
      (new-name (union taken (make-set result))))))

(define (new-var taken)
  (make-var
    (new-name (set-map name taken))))
