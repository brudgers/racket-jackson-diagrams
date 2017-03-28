#lang racket

#| Description 
Provide a configuration
Provide helpers using configuration
Provide base elements for Jackson Diagrams
|#

(require pict)

(provide sequence
         iteration
         selection
         configuration)

#| Configuration |#
(struct configuration
  (render-color
   text-style
   text-size
   text-angle
   inset
   frame-width
   back-ground)
  #:transparent)

 (define config 
   (configuration "gray" ;render-color
                  'modern   ;text-style
                  20     ;text-size
                  0      ;text-angle   
                  5      ;inset
                  3      ;frame-width
                  "cornflower blue"
                  ))    

#| Helpers |#
(define (color pict)
  (colorize pict (configuration-render-color config)))

(define (label item)
  (text item
        (configuration-text-style config)
        (configuration-text-size config)
        (configuration-text-angle config)))

(define (margins item)
  (inset item
         (configuration-inset config)))

(define (background item)
  (let 
      ((back (colorize (filled-rectangle
                       (pict-width item)
                       (pict-height item))
                       (configuration-back-ground config))))
    (cc-superimpose back item)))

(define (border item)
  (background 
   (frame item
          #:line-width (configuration-frame-width config))))

(define (node item type more)
  (tree-layout #:pict (type item) more))




#| Elements |#

(define (sequence item)
  (color (border 
          (margins
           (label item)))))

(define (selection item)
  (color (border
          (margins
           (vr-append
            (configuration-inset config)
            (circle (/ (configuration-text-size config)
                       2))
            (label item))))))

(require pict/flash)
(define (iteration item)
  (color (border
          (margins
           (vr-append
            (configuration-inset config)
            (let ((s (* .6 (configuration-text-size config))))
              (filled-flash s s 6 .8))
            (label item))))))

#| Trees |#

(require pict/tree-layout)

(define (make-subtree head-label head-type node-labels node-type)
  (naive-layered (apply tree-layout
                        #:pict (head-type head-label)
                        (map eval (map node-type node-labels)))))

(define (a-select label)
  `(node ,label selection #f))

(define (an-iteration label)
  `(node ,label iteration #f))

(define (a-sequence label)
  `(node ,label sequence #f))
