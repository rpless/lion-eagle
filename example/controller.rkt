#lang racket
(require "../lion-eagle/controller.rkt" "model.rkt")
(provide foo-controller make-foo-controller)

(define-controller foo
  (define/action (bar) 
    (set-counter! (add1 (get-counter)))))