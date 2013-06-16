(define (iterate from to fun)
  (if (not (= from to))
      (let ((v (fun from)))
        (cons v (iterate (+ from 1) to fun)))
      '()))

(define (make-matrix rows cols . val)
  (let ((m (make-vector rows 0))
        (v (if (null? val) 0 (car val))))
    (iterate 0 rows (lambda (r)
                      (vector-set! m r (make-vector cols v))))
    m))

(define (eye n)
  (let ((ret (make-matrix n n)))
    (iterate 0 n (lambda (i)
                   (matrix-set! ret i i 1)))
    ret))

(define (matrix? m)
  (and (vector? m)
       (> (vector-length m) 0)
       (vector? (vector-ref m 0))))

(define (matrix-rows m)
  (vector-length m))

(define (matrix-columns m)
  (vector-length (vector-ref m 0)))

(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j))

(define (matrix-set! m i j val)
  (vector-set! (vector-ref m i) j val))

(define (matrix-mul m1 m2)
  (let* ((rows1 (matrix-rows m1))
         (rows2 (matrix-rows m2))
         (cols1 (matrix-columns m1))
         (cols2 (matrix-columns m2))
         (ret (make-matrix rows1 cols2)))
    (if (not (= rows2 cols1))
        (error "Invalid matrix sizes."))
    (iterate 0 rows1 (lambda (i)
                       (iterate 0 cols2 (lambda (j)
                                          (let ((val 0))
                                            (iterate 0 cols1 (lambda (k)
                                                               (set! val (+ val
                                                                            (* (matrix-ref m1 i k)
                                                                               (matrix-ref m2 k j))))))
                                            (matrix-set! ret i j val))))))
    ret))


(define m1 (eye 2))
(define m2 (make-matrix 2 2 3))

(matrix-mul m1 m2)
