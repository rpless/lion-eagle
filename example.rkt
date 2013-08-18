#lang racket
(require "lion-eagle/view.rkt" "lion-eagle/model.rkt" "lion-eagle/controller.rkt")



(define-model foo (counter))
(define model (make-foo 0))

(define-controller foo ((bar (set-counter! (add1 (get-counter))))))
(define controller (make-foo-controller model))
(define-values/invoke-unit controller
  (import)
  (export foo-controller))

(define example-view (view (frame f "MVC Test"
                                  (message m1 (bind counter number->string))
                                  (textfield t1 (bind counter number->string string->number))
                                  (button b1 "Click Me" (action bar)))))
(example-view)