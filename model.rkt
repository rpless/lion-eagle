#lang racket

(require (for-syntax syntax/parse racket))

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-model)

;; define-model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-model stx)
  (syntax-case stx ()
    [(_ name [fieldid contract] ...)
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