;; lists
(define (caar list) (car (car list)))
(define (cadr list) (car (cdr list)))
(define (cdar list) (cdr (car list)))
(define (cddr list) (cdr (cdr list)))
(define (caaar list) (car (car (car list))))
(define (caadr list) (car (car (cdr list))))
(define (cadar list) (car (cdr (car list))))
(define (caddr list) (car (cdr (cdr list))))
(define (cdaar list) (cdr (car (car list))))
(define (cdadr list) (cdr (car (cdr list))))
(define (cddar list) (cdr (cdr (car list))))
(define (cdddr list) (cdr (cdr (cdr list))))
(define (caaaar list) (car (car (car (car list)))))
(define (caaadr list) (car (car (car (cdr list)))))
(define (caadar list) (car (car (cdr (car list)))))
(define (caaddr list) (car (car (cdr (cdr list)))))
(define (cadaar list) (car (cdr (car (car list)))))
(define (cadadr list) (car (cdr (car (cdr list)))))
(define (caddar list) (car (cdr (cdr (car list)))))
(define (cadddr list) (car (cdr (cdr (cdr list)))))
(define (cdaaar list) (cdr (car (car (car list)))))
(define (cdaadr list) (cdr (car (car (cdr list)))))
(define (cdadar list) (cdr (car (cdr (car list)))))
(define (cdaddr list) (cdr (car (cdr (cdr list)))))
(define (cddaar list) (cdr (cdr (car (car list)))))
(define (cddadr list) (cdr (cdr (car (cdr list)))))
(define (cdddar list) (cdr (cdr (cdr (car list)))))
(define (cddddr list) (cdr (cdr (cdr (cdr list)))))

(define (list . objs) objs)

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))

(define (reverse list)
  (foldl (lambda (acc x)
           (cons x acc))
         '() list))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (- k 1))))

(define (memq obj list)
  (if (null? list)
      #f
      (if (eq? (car list) obj)
          list
          (memq obj (cdr list)))))

(define (memv obj list)
  (if (null? list)
      #f
      (if (eqv? (car list) obj)
          list
          (memv obj (cdr list)))))

(define (member obj list)
  (if (null? list)
      #f
      (if (equal? (car list) obj)
          list
          (member obj (cdr list)))))

(define (assq obj alist)
  (if (null? alist)
      #f
      (if (eq? (caar alist) obj)
          (car alist)
          (assq obj (cdr alist)))))

(define (assv obj alist)
  (if (null? alist)
      #f
      (if (eqv? (caar alist) obj)
          (car alist)
          (assv obj (cdr alist)))))

(define (assoc obj alist)
  (if (null? alist)
      #f
      (if (equal? (caar alist) obj)
          (car alist)
          (assoc obj (cdr alist)))))

;; characters
(define (char-ci=? char1 char2)
  (char=? (char-downcase char1)
          (char-downcase char2)))

(define (char-ci<? char1 char2)
  (char<? (char-downcase char1)
          (char-downcase char2)))

(define (char-ci>? char1 char2)
  (char>? (char-downcase char1)
          (char-downcase char2)))

(define (char-ci<=? char1 char2)
  (char<=? (char-downcase char1)
           (char-downcase char2)))

(define (char-ci>=? char1 char2)
  (char>=? (char-downcase char1)
           (char-downcase char2)))

;; strings
(define (string . args)
  (apply list->string (list args)))

;; numbers
(define (+ . lst)
  (foldl __add 0 lst))

(define (- . lst)
  (define l (length lst))
  (cond ((= l 0) (error "Invalid number of arguments."))
        ((= l 1) (__sub 0 (car lst)))
        (else (foldl __sub (car lst) (cdr lst)))))

(define (- . lst)
  (let ((l (length lst)))
    (cond ((= l 0) (error "Invalid number of arguments."))
        ((= l 1) (__sub 0 (car lst)))
        (else (foldl __sub (car lst) (cdr lst))))))

(define (* . lst)
  (foldl __sub 0 lst))

(define (zero? num) (= 0 num))
(define (positive? num) (> num 0))
(define (negative? num) (< num 0))
(define (odd? num) (= (modulo num 2) 1))
(define (even? num) (= (modulo num 2) 0))

(define (min . lst)
  (foldl (lambda (acc el)
           (if (< el acc)
               el
               acc))
         (car lst) (cdr lst)))

(define (max . lst)
  (foldl (lambda (acc el)
           (if (> el acc)
               el
               acc))
         (car lst) (cdr lst)))

;; control
(define (map fun list)
  (if (null? list)
      '()
      (cons (fun (car list))
            (map fun (cdr list)))))

(define (foldl fun acc list)
  (if (null? list)
      acc
      (foldl fun (fun acc (car list)) (cdr list))))

(define (foldr fun acc list)
  (if (null? list)
      acc
      (fun (car list) (foldr fun acc (cdr list)))))

(define (for-each fun list)
  (if (null? list)
      '()
      (begin (fun (car list))
             (for-each fun (cdr list)))))

(define (force promise)
  (promise))

;; io
(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename))
        (v (proc port)))
    (close-input-port port)
    v))

(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename))
        (v (proc port)))
    (close-input-port port)
    v))

(define (call-with-output-file filename proc)
  (define port (open-output-file filename))
  (define v (proc port))
  (close-output-port port)
  v)

(define (newline . port)
  (if (null? port)
      (write-char #\newline)
      (write-char #\newline (car port))))

