#lang racket

(require (for-syntax syntax/parse racket "utilities.rkt" "view.rkt"))
(require "model.rkt" "controller.rkt" racket/gui/base)

;; MVC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide mvc)

(define-syntax (mvc stx)
  (syntax-parse stx
    [(_ (model mname:id [field:id val] ...)
        (view (frame title:expr component:textfield))
        (controller (action:id impl:expr ...) ...))
     (define controller-name (datum->syntax stx (symbol-append (syntax->datum #'mname) '-controller)))
     #`(let () 
         ;; Create the Model
         (define-model mname [#,@#`(field ...)])
         (define foo (new mname #,@(for/list ([n (syntax->datum #'(field ...))]
                                              [v (syntax->datum #'(val ...))])
                                     #`[#,(datum->syntax stx n) #,(datum->syntax stx v)])))
         
         ;; Create the View and binders
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (define frame (new frame% [label ""]))
         (define #,#'component.name (new text-field% [label ""] [parent frame]))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                  
         
         ;; Create the Controller
         (define-controller #,controller-name [action (thunk (begin impl ...))] ... 
           [field ...]
           [#,#'component.field (λ (new-value) (send #,#'component.name set-value (#,#'component.func new-value)))])
         (values (new #,controller-name [model foo]) frame))]))