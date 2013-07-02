#lang racket

(require racket/gui)
(require (for-syntax syntax/parse racket "utilities.rkt"))
(require "model.rkt" "controller.rkt")

;; MVC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide mvc)

(define-syntax (mvc stx)
  (syntax-parse stx
    [(_ (model mname:id [field:id val] ...)
        (view (frame (textfield text:id initial:expr (bind fieldid:id func:expr)))) ;; this is essentially hardcoded. need to change that
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
         (define frame (new frame% [label "Test"]))
         (define tf (new text-field% [label ""] [parent frame]))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                  
         
         ;; Create the Controller
         (define-controller #,controller-name [action (thunk (begin impl ...))] ... [field ...] [fieldid (λ (new-value) (send tf set-value (func new-value)))])
         (values (new #,controller-name [model foo]) frame))]))