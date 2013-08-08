#lang racket
(require (for-syntax syntax/parse racket "private/utilities.rkt")
         racket/unit)

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-model stx)
  (syntax-parse stx
    [(_ name:id (field:id ...))
     (define fields (syntax->datum #'(field ...)))
     (define gen-fields (map (λ (f) (datum->syntax stx (gensym f))) fields))
     (define getters (map (compose (curry datum->syntax stx) (curry symbol-append 'get-)) fields))
     (define setters (map (compose (curry datum->syntax stx) (λ (f) (symbol-append 'set- f '!))) fields))
     (define sig-name (datum->syntax stx (symbol-append (syntax->datum #'name) '^)))
     #`(begin
         (define-signature #,sig-name
           (#,@getters #,@setters))
         (define name 
           (λ (#,@gen-fields)
             (unit
               (import)
               (export #,sig-name)
               
               #,@(map (λ (f g) #`(define #,(datum->syntax stx f) #,g)) fields gen-fields)
               #,@(map (λ (f g) #`(define (#,g) #,(datum->syntax stx f))) fields getters)
               #,@(map (λ (f s) #`(define (#,s val) (set! #,(datum->syntax stx f) val))) fields setters)))))]))

(define-model foo (bar))