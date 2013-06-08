#!r6rs

(import
  (lambda-calculator tokenize)
  (lambda-calculator parse)
  (lambda-calculator reduce)
  (lambda-calculator print))

(display
  (print
    (reduce-norm
      (parse
        (tokenize
          (string->list
            (get-string-all
              (transcoded-port
                (standard-input-port)
                (make-transcoder
                  (utf-8-codec)
                  (eol-style lf))))))))))
(newline)

(exit)
