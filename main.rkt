#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme inexact)
                
        (only (racket base) random-seed)
        (only (racket class) send)
        (only (racket draw) the-color-database)
        (only (racket list) shuffle)
        (only (racket mpair) list->mlist mlist->list)
        
        (prefix (2htdp image) i:)
        (only (pict) show-pict scale)
        
        (prefix (a-d graph weighted config) wg:)
        (a-d segmentation)
        (a-d scheme-tools))

; image->graph
; ( string ➙ weighted-graph )
; Given a path to an image file, this procedure creates a weighted graph representing the given image monochromatically (grayscale).
; This procedure internally uses a neighborhood function to link nodes together.
(define (image->graph p)
  (define img (i:bitmap/file p))
  (define height (i:image-height img))
  (define width (i:image-width img))

  ; Transforms RGB values into a monochromatic (gray) value by calculating a weighted average.
  ; Source for weight constants: https://en.wikipedia.org/wiki/Grayscale#Converting_color_to_grayscale
  (define (to-mono color) 
    (exact (round (+ (* (i:color-red color) 0.2126)
                     (* (i:color-green color) 0.7152)
                     (* (i:color-blue color) 0.0722)))))

  (define pixels (list->vector (map to-mono (list->mlist (i:image->color-list img))))) ; row-major ordening
  (define g (wg:new #f (* height width)))
  (wg:for-each-node g
                    (lambda (node)
                      (for-each (lambda (neighbor)
                                  (wg:add-edge! g node neighbor (abs (- (vector-ref pixels node)
                                                                        (vector-ref pixels neighbor)))))
                                (neighborhood node height width))))
  g)

; neighborhood
; ( number number number ➙ pair )
; Graph neighborhood function.
; Returns a list of nodes surrounding a given node, considering the graph nodes represent the pixels of an image.
; The height and width of the image (in terms of pixels) need to be provided.
(define (neighborhood n height width)
  (define make-coord cons)
  (define row car)
  (define col cdr)
  
  (define (coord->node coord)
    (+ (* (row coord) width)
       (col coord)))
  
  (define (in-bounds? coord)
    (and (< -1 (row coord) height)
         (< -1 (col coord) width)))
  
  (let ((row-n (quotient n height))
        (col-n (modulo n width)))
    (map coord->node (filter in-bounds?
                             (list (make-coord row-n (+ col-n 1))
                                   (make-coord (+ row-n 1) (- col-n 1))
                                   (make-coord (+ row-n 1) col-n)
                                   (make-coord (+ row-n 1) (+ col-n 1)))))))

; run-segmentation
; ( string ➙ pair )
; Given a string reffering to an image file, this procedure loads the file as a graph and performs a segmentation.
; Returns a pair containing the number of components resulting from the segmentation (car)
; and a node-indexed vector (cdr) mapping the nodes to their corresponding component.
(define (run-segmentation path)
  (segment (image->graph path)))

; show-segmentation
; ( pair ➙ ∅ )
; This procedure makes a drawing of a segmentation.
; The output of the run-segmentation procedure can thus be provided as input for this procedure to visualise the segmentation.
; This procedure assumes the image's resolution is a square (i.e. equal width and height).
(define (show-segmentation components)
  (define num-comps (car components))
  (define seg-vec (cdr components))
  (define dim-siz (sqrt (vector-length seg-vec)))
  (define color-names (let ()
                        (random-seed 54321)
                        (list->vector (list->mlist (shuffle (send the-color-database get-names))))))
  (define seg-img (i:color-list->bitmap (mlist->list (map (lambda (mono)
                                                            (vector-ref color-names mono))
                                                          (vector->list seg-vec)))
                                        dim-siz dim-siz))
  ; The visualisation draws an enlarged image for eye comfort.
  ; This may look blurry due to the underlying upscaling technique in the image library.
  ; To see the image in its actual resolution, call (show-pict seg-img) instead.
  (show-pict (scale seg-img 4))) 

; examples
; A list of strings referring to relative paths leading to images.
(define examples '("examples/1.png"
                   "examples/6.png"
                   "examples/7.png"
                   "examples/9.png"
                   "examples/10.png"
                   "examples/13.png"
                   "examples/15.png"
                   ))
; test
; ( pair ➙ ∅ )
; Procedure to run a list of paths in 
(define (test paths)
  (for-each (lambda (path)
              (let ((seg-result (run-segmentation path)))
                (show-segmentation seg-result)))
            paths))

(test examples)
