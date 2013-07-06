#lang racket

(require (for-syntax syntax/parse racket))

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-model)

;; define-model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-model stx)
  (syntax-parse stx
    [(_ name:id [fieldid:id contract:expr] ...)
     #`(begin 
         (define/contract name
           (class/c 
            (field #,@(for/list ([f (syntax->datum #'(fieldid ...))]
                                 [c (syntax->datum #'(contract ...))])
                        #`[#,f #,c])))
           (class object% 
             (super-new)
             (inspect #f)
             (init-field #,@#`(fieldid ...)))))]))