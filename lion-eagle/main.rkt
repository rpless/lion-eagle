#lang racket

(require (for-syntax syntax/parse racket "private/utilities.rkt"))
(require "model.rkt" "controller.rkt" "view.rkt")

;; MVC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide mvc)

(define-syntax (mvc stx)
  (syntax-parse stx
    [(_ (model mname:id field-spec ...)
        (view ui)
        (controller (action:id impl:expr ...) ...))
     (define controller-name (datum->syntax stx (symbol-append (syntax->datum #'mname) '-controller)))
     (define controlled-fields (datum->syntax stx (map extract-id (syntax->datum #'(field-spec ...)))))
     #`(λ (x . y) 
         ;; Create the Model
         (define-model mname field-spec ...)
         (define mod (apply make-object mname x y))
         
         ;; Create the Controller
         (define-controller #,controller-name #,(values controlled-fields) [action (begin impl ...)] ...)
         (define control (new #,controller-name [model mod]))

         (values control (component control ui)))]))