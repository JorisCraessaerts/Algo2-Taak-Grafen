#lang r7rs

(define-library (image-graph-segmentation)
  (export segment) ; DO NOT CHANGE THIS LINE!
  (import (scheme base)
          (a-d weighted-graph)
          (prefix (a-d disjoint-sets optimized) dset:)) ; Add any necessary library to implement your assignment here.
                         ; Only R⁷RS-libraries, sublibraries from the course (a-d) and WPO solutions are allowed! 

  (begin

    ; segment
    ; ( weighted-graph ➙ pair )
    ; Performs the segmentation of a given weighted graph.
    ; Returns a pair containing the number of components resulting from the segmentation (car)
    ; and a node-indexed vector (cdr) mapping the nodes to their corresponding component.
    (define (segment g)
      (define k 150)
      (define segmentation (initial-segmentation))



      (define (edges-with-weights)
        
        )

      (define (initial-segmentation)
        (dset:new (order g)))


      ; Code to create a sorted list of edges of a graph
      (define (n1 edge) (car edge)) ; Abstraction to get node 1 from an edge
      (define (n2 edge) (cadr edge)) ;Abstraction to get node 2 from an edge
      (define (weight edge) (caddr edge))


      (define (all-edges-list graph)
        (let ((edges '())
              (visited (make-vector (order graph) #f)))
          (for-each-node graph
                         (lambda (node)
                           (vector-set! visited node #t)
                           (for-each-edge graph node
                                          (lambda (w to)
                                            (unless (vector-ref visited to)
                                              (set! edges (cons (list node to w) edges)))))))
          (sort edges (lambda (edge1 edge2)
                        (< (weight edge1) (weight edge2))))))
      

      )

    

))
    