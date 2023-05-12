#lang r7rs

(define-library (image-graph-segmentation)
  (export segment) ; DO NOT CHANGE THIS LINE!
  (import (scheme base)
           (a-d weighted-graph)) ; Add any necessary library to implement your assignment here.
                         ; Only R⁷RS-libraries, sublibraries from the course (a-d) and WPO solutions are allowed! 

  (begin

    ; segment
    ; ( weighted-graph ➙ pair )
    ; Performs the segmentation of a given weighted graph.
    ; Returns a pair containing the number of components resulting from the segmentation (car)
    ; and a node-indexed vector (cdr) mapping the nodes to their corresponding component.
    (define (segment g)
      (define segmentation (initial-segmentation))

      (define (initial-segmentation)
        (define seg (make-vector (order g) 0))
        (define size (vector-length seg))
        (do ([i 0 (+ i 1)])
          ((= i size) seg)
          (vector-set! seg i i))
        (seg))

      )

    

))
    