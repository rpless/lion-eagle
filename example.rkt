#lang racket
(require "mvc.rkt")

(define-values (x view)
  (mvc (model foo [counter 0])
       (view (frame window "MVC Test"
                    (message m1 (bind counter number->string))
                    (textfield t1 (bind counter number->string string->number))
                    (button b1 "Click Me" (action increment))))
       (controller (increment (send this set-counter (add1 (send this get-counter)))))))
(send view show #t)