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
         (define mod (new mname #,@(for/list ([n (syntax->datum #'(field ...))]
                                              [v (syntax->datum #'(val ...))])
                                     #`[#,(datum->syntax stx n) #,(datum->syntax stx v)])))
         
         ;; Create the Controller
         (define-controller #,controller-name 
           [field ...]
           [action (thunk (begin impl ...))] ...)
         (define control (new #,controller-name [model mod]))
         
         ;; Create the View and binders
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (define frame (new frame% [label ""]))
         (define #,#'component.name (new text-field% [label ""]
                                         [parent frame]
                                         [callback (λ (self evt) (if (eq? (send evt get-event-type) 'text-field-enter)
                                                                     (send control #,(datum->syntax stx (symbol-append 'set- (syntax->datum #'component.field)))
                                                                           (#,#'component.textfield-> (send self get-value)))
                                                                     (void)))]))
         (send control #,(datum->syntax stx (symbol-append 'add-notifer: (syntax->datum #'component.field)))
               (λ (new-value) (send #,#'component.name set-value (#,#'component.->textfield new-value))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                  
         
         (values control frame))]))