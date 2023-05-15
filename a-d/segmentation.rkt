#lang r7rs

(define-library (image-graph-segmentation)
  (export segment) ; DO NOT CHANGE THIS LINE!
  (import (scheme base)
          (racket/hash) ; https://docs.racket-lang.org/reference/hashtables.html
          (a-d graph weighed config) ; I use the adjecency list representation. Pictures can contain millions of pixels but each pixel is only connected to max 8 other pixels surrounding itself. This makes a sparse graph.
          (prefix (a-d disjoint-sets optimized) dset:))
  ; Add any necessary library to implement your assignment here.
  ; Only R⁷RS-libraries, sublibraries from the course (a-d) and WPO solutions are allowed! 

  (begin
    ; segment
    ; ( weighted-graph ➙ pair )
    ; Performs the segmentation of a given weighted graph.
    ; Returns a pair containing the number of components resulting from the segmentation (car)
    ; and a node-indexed vector (cdr) mapping the nodes to their corresponding component.
    (define (segment g)

      ; Setup
      ; -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      (define k 150)
      (define n (order g))
      (define component-sizes (make-hash)) ; Hash table that keeps tracks of the sizes of the components so we don't have to recalculate the sizes of the components each time we calculate their internal difference.

      ; Abstractions for edges
      ; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
      (define (n1 edge) (car edge)) ; Abstraction to get node 1 from an edge
      (define (n2 edge) (cadr edge)) ; Abstraction to get node 2 from an edge
      (define (weight edge) (caddr edge)) ; Abstraction to get the weight from an edge

      
      ; 1) Step 1: Create initial segmentation where every node is in its own component
      ;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
      (define (initial-segmentation size) ; Procedure to create the inital segmentation where every node is in its own segment
        (let ((dsets (dset:new size)))
          (do ((i 0 (+ i 1))) ((= i size))
            (hash-set! component-sizes i 1)) ; In de hashtabel gaan we voor elke component de value (de bijhorende size dus) op 1 zetten omdat een component initieel maar uit één node bestaat.
          dsets))
      
      (define segmentation (initial-segmentation n))

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
      
      (define sorted-edges (sorted-edge-list g))

      ; Some helper procedures to calculate the internal weight of components and to merge 2 components etc...
      ; -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
      (define (i c)
        (if (= (hash-ref component-sizes c) 1)
            0
            (let ((max-weight 0)) ; We gaan over de nodes van de graaf 
              (for-each-node g
                             (lambda (node)
                               (when (= (dset:find segmentation node) c) ; if node is in component c
                                 (for-each-edge g node
                                                (lambda (w to)
                                                  (when (and (= (dset:find segmentation to) c) ; if "to" node is in component c
                                                             (> w max-weight)) ; and weight is larger than max so far
                                                    (set! max-weight w))))))) ; update the max weight
              max-weight)))

      (define (t c)
        (let ((component-size (hash-ref component-sizes c)))
          (/ k component-size)))

      (define (component-sum c)
        (+ (i c) (t c)))

      ; Code om twee componenten te mergen. Belangrijk hierbij is dat door components te mergen het aantal nodes in deze componenten ook zal veranderen. We moeten dus ook de hashtabel updaten die we gebruiken voor het bijhouden van de sizes van de verschillende componenten.
      (define (merge-components c1 c2)
        (let* ((updated-segmentation (dset:union! segmentation c1 c2))
               (new-component (dset:find updated-segmentation c1)) ; get the representative of the new component
               (new-size (+ (hash-ref component-sizes c1 0) (hash-ref component-sizes c2 0))))
          (hash-remove! component-sizes c1)
          (hash-remove! component-sizes c2)
          (hash-set! component-sizes new-component new-size)
          updated-segmentation))

      ; Some Procedures to create the final result
      ; -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      (define (component-vector)
        (define node-indexed-vector (make-vector n))
        (for-each
         (lambda (node)
           (vector-set! node-indexed-vector node (dset:find segmentation node)))
         (range n))
        node-indexed-vector)
     
      (define (number-of-components)
        (define component-set (make-hash))
        (for-each
         (lambda (node)
           (hash-set! component-set (vector-ref node-indexed-vector node) #t))
         (range n))
        (hash-count component-set))

      (define (algorithm-result)
        (cons (number-of-components) (component-vector)))    

      ; Main algorithm logic
      ; -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      (for-each
       (lambda (edge)
         (let* ((from (n1 edge))
                (to (n2 edge))
                (edge-weight (weight edge))
                (from-component (dset:find segmentation from))
                (to-component (dset:find segmentation to)))
           (if (and (not (= from-component to-component)) ; from-component and to-component are different
                    (< edge-weight (min (component-sum from-component) (component-sum to-component)))) ; edge-weight is smaller than the minimum of (component-sum from-component) and (component-sum to-component)
               (set! segmentation (merge-components from-component to-component)))))  
       sorted-edges)

      (algorithm-result)
      )))
  