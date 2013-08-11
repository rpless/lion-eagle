#lang racket
(require "lion-eagle/view.rkt" "lion-eagle/model.rkt" "lion-eagle/controller.rkt")



(define-model foo (counter))
(define model (make-foo 0))

(define-controller foo)
(define controller (make-foo-controller model))
(define-values/invoke-unit (make-foo-controller model)
  (import)
  (export foo-controller))

(define-view (frame f "MVC Test"
                    (textfield t1 (bind counter number->string string->number))))
