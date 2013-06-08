(library (rename)
  (export new-name)
  (import
    (rnrs base)
    (lambda-calculator set))

  (define low-char #\a)
  (define hi-char #\z)

  (define (next-char c)
    (integer->char (+ (char->integer c) 1)))

  (define (seed-string len)
    (if (= len 1)
      (string low-char)
      (string-append (seed-string 1) (seed-string (- len 1)))))

  (define (next-string str)
    (let* ([len (string-length str)]
           [last-index (- len 1)]
           [last-char (string-ref str last-index)])
      (if (char=? last-char hi-char)
        (seed-string (+ len 1))
        (string-append
          (substring str 0 last-index)
          (string (next-char last-char))))))

  (define (new-name taken)
    (define (helper current)
      (if (not (elem? current taken))
        current
        (helper (next-string current))))
    (helper (seed-string 1))))
