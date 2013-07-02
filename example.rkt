#lang racket
(require "mvc.rkt")

(define-values (x view)
  (mvc (model foo [counter 5])
       (view (frame (textfield f1 "" (bind counter number->string))))
       (controller (hello (displayln "Hello")
                          (displayln "World")))))