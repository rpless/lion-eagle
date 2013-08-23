#lang racket
(require "../lion-eagle/controller.rkt" "model.rkt")
(provide count-model-controller make-count-model-controller)

(define-controller count-model
  (define/action (update-count) 
    (set-count! (add1 (get-count)))))