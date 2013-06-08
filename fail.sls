#!r6rs

(library (fail)
  (export fail)
  (import
    (rnrs base)
    (rnrs io simple)
    (rnrs programs))

  (define (fail str)
    (display str)
    (newline)
    (exit 1)))
