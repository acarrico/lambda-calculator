(load "set.scm")

(define (next-char c)
  (case c
    [#\z #\a]
    [else (integer->char (+ (char->integer c) 1))]))

(define (next-string str)
  (case str
    ["" "a"]
    [else
     (let* ([lst (reverse (string->list str))]
            [head (car lst)])
      (list->string
        (reverse
          (case head
            [#\z
             (cons #\a lst)]
            [else
             (cons (next-char head) (cdr lst))]))))]))

(define (new-name taken)
  (let ([result (next-string "")])
    (if (not (elem? result taken))
      result
      (new-name (union taken (make-set result))))))

(define (new-var taken)
  (make-var
    (new-name (set-map name taken))))
