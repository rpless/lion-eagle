#lang racket

(require (for-syntax syntax/parse racket)
         "utilities.rkt")

;; Controller Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-controller)

;; define-controller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-controller stx)
  (syntax-parse stx
    [(_ name:id [action:id impl] ... [delegate:id ...] [bind:id callback:expr] ...)
     (define getters (map (curry symbol-append 'get-) (syntax->datum #'(delegate ...))))
     (define setters (map (curry symbol-append 'set-) (syntax->datum #'(delegate ...))))
     #`(begin 
         (define name 
           (class object%
             (super-new)
             (inspect #f)
             
             (init model)
             (define in:model model)
             
             ;; actions
             #,@(for/list ([a (syntax->datum #'(action ...))]
                           [i (syntax->datum #'(impl ...))])
                  #`(define/public (#,(datum->syntax stx a)) (#,i)))
             
             ;; TODO: Notifiers for the view
             #,@(for/list ([s setters]
                           [g getters]
                           [f (syntax->datum #'(delegate ...))])
                  #`(begin 
                      (define #,(datum->syntax stx (symbol-append 'notifers: f)) 
                        (list #,@(for/list ([b (syntax->datum #'(bind ...))]
                                            [c (syntax-e #'(callback ...))]
                                            #:when (equal? b f))
                                   c)))
                      
                      (define/public (#,(datum->syntax stx s) value)
                        (set-field! #,(datum->syntax stx f) in:model value)
                        (for ([n #,(datum->syntax stx (symbol-append 'notifers: f))])
                          (n value)))
                      
                      (define/public (#,(datum->syntax stx g))
                        (get-field #,(datum->syntax stx f) in:model)))))))]))
