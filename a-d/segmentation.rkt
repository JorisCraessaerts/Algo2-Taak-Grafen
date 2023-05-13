#lang r7rs

(define-library (image-graph-segmentation)
  (export segment) ; DO NOT CHANGE THIS LINE!
  (import (scheme base)
          (racket/hash) ; https://docs.racket-lang.org/reference/hashtables.html
          (a-d graph weighed config) ; I use the adjecency list representation. Pictures can contain millions of pixels but each pixel is only connected to max 8 other pixels surrounding itself. This makes a sparse graph.
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
      (define sorted-edges (sorted-edge-list g))



      (define (initial-segmentation)
        (dset:new (order g)))


      ; Abstractions for edges
      (define (n1 edge) (car edge)) ; Abstraction to get node 1 from an edge
      (define (n2 edge) (cadr edge)) ; Abstraction to get node 2 from an edge
      (define (weight edge) (caddr edge)) ; Abstraction to get the weight from an edge
      
      ; Code to create a sorted list of edges of a graph
      (define (sorted-edge-list graph)
        (define (edge-has-been-visited visited-edges edge) ; We are working with undirected graphs. This means that every time we see an edge (n1 n2 "some-weight"), we have to check that the same edge (n2 n1 "some-weight") doesn't already exist. I did this by keeping track of a hash table that for every node keeps track of all other nodes it is connected to.
          (let ((n1 (n1 edge))
                (n2 (n2 edge)))
            (or (member n2 (hash-ref visited-edges n1 '())) ; If n2 is in the list of visited nodes of n1, this means the edge has been visited.
                (member n1 (hash-ref visited-edges n2 '()))))) ; If n1 is in the list of visited nodes of n2, this means the edge has been visited.
        
        (let ((edges '())
              (visited-edges (make-hash))) ; make hash is a procedure from racket to create new hashtables. This hashtable is created to keep track of the visited edges (so we are not working with doubles in our undirected graph)
          (for-each-node graph
                         (lambda (from)
                           (for-each-edge graph from
                                          (lambda (w to)
                                            (let ((edge (list from to w)))
                                              (unless (edge-has-been-visited visited-edges edge)
                                                (hash-update! visited-edges from (lambda (old) (cons to old)) '()) ; We add the "to-node" of the edge to the list of edges the "from-node" is connected to. (the list in the hash-table for element "from-node"). We do this by consing the "to-node" to the existing list in the hash table
                                                (hash-update! visited-edges to (lambda (old) (cons from old)) '()) ; We add the "from-node" of the edge to the list of edges the "to-node" is connected to. (the list in the hash-table for element "to-node"). We do this by consing the "from-node" to the existing list in the hash table
                                                (set! edges (cons edge edges))))))) ; If the edge is not visited yet, we add it to the list of edges that will be sorted.
                         (sort edges (lambda (edge1 edge2)
                                       (< (weight edge1) (weight edge2)))))))

    
      )

))
    