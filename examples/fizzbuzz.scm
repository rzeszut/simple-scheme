(define (write-string str port)
  (for-each (lambda (c) (write-char c port)) (string->list str)))

(define (iterate from to fun)
  (if (not (= from to))
      (let ((v (fun from)))
        (cons v (iterate (+ from 1) to fun)))
      '()))

(define (print-fizz-buzz i port)
  (cond ((= 0 (modulo i 3)) (write-string "fizz" port))
        ((= 0 (modulo i 5)) (write-string "buzz" port))
        ((= 0 (modulo i 15)) (write-string "fizzbuzz" port))
        (else #f)))

(define (fizz-buzz . p)
  (let ((port (if (null? p) (current-output-port) p)))
    (iterate 1 100 (lambda (i)
                     (write-string (number->string i) port)
                     (write-char #\space port)
                     (print-fizz-buzz i port)
                     (newline port)))))

(fizz-buzz)
