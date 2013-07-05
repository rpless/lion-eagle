#lang racket
(require "mvc.rkt" racket/gui/base)

(define-values (x view)
  (mvc (model foo [counter 0])
       (view (frame "MVC Test"
                    (textfield t1 (bind counter number->string string->number))
                    (button b1 "Click Me" (action hello))))
       (controller (hello (send this set-counter (add1 (send this get-counter)))))))