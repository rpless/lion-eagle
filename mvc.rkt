#lang racket

(require (for-syntax syntax/parse racket "utilities.rkt"))
(require "model.rkt" "controller.rkt" "view.rkt" racket/gui/base)

;; MVC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide mvc)

(define-syntax (mvc stx)
  (syntax-parse stx
    [(_ (model mname:id [field:id val] ...)
        (view (frame title:expr comp))
        (controller (action:id impl:expr ...) ...))
     (define controller-name (datum->syntax stx (symbol-append (syntax->datum #'mname) '-controller)))
     #`(let () 
         ;; Create the Model
         (define-model mname [#,@#`(field ...)])
         (define mod (new mname #,@(for/list ([n (syntax->datum #'(field ...))]
                                              [v (syntax->datum #'(val ...))])
                                     #`[#,(datum->syntax stx n) #,(datum->syntax stx v)])))
         
         ;; Create the Controller
         (define-controller #,controller-name 
           [field ...]
           [action (thunk (begin impl ...))] ...)
         (define control (new #,controller-name [model mod]))
         
         ;; Create the View and binders
         (define parent (new frame% [label title]))
         (component control parent comp)               
         
         (values control parent))]))