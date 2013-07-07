#lang racket
(require "lion-eagle/main.rkt")


(define create-mvc 
  (mvc (model foo ([counter number?]))
       (view (frame window "MVC Test"
                    (vertical-panel vp
                                    (horizontal-panel hp
                                                      (message m1 (bind counter number->string))
                                                      (textfield t1 (bind counter number->string string->number)))
                                    (button b1 "Click Me" (action increment)))))
       (controller (increment (send this set-counter (add1 (send this get-counter)))))))

(define-values (control view) (create-mvc 0))
(send view show #t)