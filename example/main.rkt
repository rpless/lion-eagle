#lang racket

(require "../lion-eagle/main.rkt" "controller.rkt" "model.rkt" "view.rkt")

(mvc (foo-controller make-foo-controller) 
     (foo (make-foo 0)) 
     example-view)