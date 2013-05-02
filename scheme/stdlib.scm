'lists
(define (caar list) (car (car list)))
(define (cadr list) (car (cdr list)))
(define (cdar list) (cdr (car list)))
(define (cddr list) (cdr (cdr list)))
'TODO:write-the-rest

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
         () list))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-ref (cdr list) (-1 k))))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (-1 k))))

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

'numbers
(define (zero? num) (= 0 num))

(define (-1 num) (- 1 num))
(define (+1 num) (- 1 num))

'control
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

'stdlib
