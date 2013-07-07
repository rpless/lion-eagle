#lang racket

(require (for-syntax syntax/parse racket "utilities.rkt"))

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-model)

;; define-model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-model stx)
  (syntax-case stx ()
    [(_ name fields ...)
     #`(begin 
         (define/contract name
           (class/c 
            (field fields ...))
           (class object% 
             (super-new)
             (inspect #f)
             (init-field #,@(map extract-id (syntax->datum #'(fields ...)))))))]))

