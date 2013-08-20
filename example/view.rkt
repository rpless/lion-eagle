#lang racket
(require "controller.rkt" "../lion-eagle/view.rkt")
(provide example-view)

(define example-view 
  (view foo-controller 
        (frame f "MVC Test"
               (message m1 (bind counter number->string))
               (textfield t1 (bind counter number->string string->number))
               (button b1 "Click Me" (action bar)))))