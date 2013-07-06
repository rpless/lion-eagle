#lang racket

(require (for-syntax syntax/parse racket "utilities.rkt"))
(require "model.rkt" "controller.rkt" "view.rkt")

;; MVC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide mvc)

(define-syntax (mvc stx)
  (syntax-parse stx
    [(_ (model mname:id [field:id contract:expr] ...)
        (view ui)
        (controller (action:id impl:expr ...) ...))
     (define controller-name (datum->syntax stx (symbol-append (syntax->datum #'mname) '-controller)))
     #`(λ (x . y) 
         ;; Create the Model
         (define-model mname [field contract] ...)
         (define mod (apply make-object mname x y))
         
         ;; Create the Controller
         (define-controller #,controller-name [field ...] [action (thunk (begin impl ...))] ...)
         (define control (new #,controller-name [model mod]))

         (values control (component control ui)))]))