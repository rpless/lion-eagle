#lang racket

(require (for-syntax syntax/parse racket))

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-model)

;; define-model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-model stx)
  (syntax-parse stx
    [(_ name:id [field:id ...])
     #`(define name
         (class object% 
           (super-new)
           (inspect #f)
           (init-field #,@#`(field ...))))]))  