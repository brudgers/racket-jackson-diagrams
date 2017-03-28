#lang racket
(require pict
         rackunit
         rackunit/text-ui
        "../src/base-elements.rkt")

(check-equal? (sequence "item1")
              (frame (text "item1"))
              "sequence test")
