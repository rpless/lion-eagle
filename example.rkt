#lang racket
(require "mvc.rkt")

(define-values (x view)
  (mvc (model foo [counter 5])
       (view (frame "MVC Test" (textfield t1 (bind counter number->string string->number))))
       (controller (hello (displayln "Hello")
                          (displayln "World")))))