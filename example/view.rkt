#lang racket
(require "controller.rkt" "../lion-eagle/view.rkt")
(provide example-view)

(define example-view 
  (view count-model-controller 
        (frame f "MVC Test"
               (message m1 (bind count number->string))
               (textfield t1 (bind count number->string string->number))
               (button b1 "Click Me" (action update-count)))))