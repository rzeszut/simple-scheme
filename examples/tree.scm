(define (write-n-spaces n port)
  (if (not (= n 0))
      (begin (write-char #\space port)
             (write-n-spaces (- n 1) port))))

(define (tree-make-node val . children)
  (list val children))

(define (tree-value tree)
  (car tree))

(define (tree-children tree)
  (cadr tree))

(define (tree-print tree . p)
  (let* ((port (if (null? p) (current-output-port) p))
         (print-with-offset (lambda (node offset)
                              (write-n-spaces offset port)
                              (if (not (= offset 1))
                                  (write-char #\-))
                              (display (tree-value node) port)
                              (for-each (lambda (n)
                                          (print-with-offset n (+ offset 2)))
                                        (tree-children node)))))
    (print-with-offset tree 0)))

(define *example-tree* (tree-make-node "root"
                                       (tree-make-node "child1"
                                                       (tree-make-node "grandchild1")
                                                       (tree-make-node "grandchild2"))
                                       (tree-make-node "child2"
                                                       (tree-make-node "grandchild3" (tree-make-node "grandgrandchild1"))
                                                       (tree-make-node "grandchild4"))
                                       (tree-make-node "child3")))

(tree-print *example-tree*)
