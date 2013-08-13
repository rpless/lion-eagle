#lang racket
(require (for-syntax syntax/parse racket "private/utilities.rkt")
         racket/unit)

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module provides the ability to define models. Defining a model creates
;; two things: a signature and a constructor. The signature specifies getters
;; and setters for all of the fields in the model. The constructor is a closure
;; that takes the same number of arguments as the there are fields in the model.
;; When invoked, the constructor returns a unit that implements the signature.
;; The constructor is named in the form make-<model name>.

(provide define-model)

;; Define Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-model stx)
  (syntax-parse stx
    [(_ name:id (field:id ...))
     (define fields (syntax->datum #'(field ...)))
     (define gen-fields (map (λ (f) (datum->syntax stx (gensym f))) fields))
     (define getters (map (compose (curry datum->syntax stx) (curry symbol-append 'get-)) fields))
     (define setters (map (compose (curry datum->syntax stx) (λ (f) (symbol-append 'set- f '!))) fields))
     #`(begin
         (define-signature name (#,@getters #,@setters))
         (define #,(datum->syntax stx (symbol-append 'make- (syntax->datum #'name)))
           (λ (#,@gen-fields)
             (unit
               (import)
               (export name)
               
               #,@(map (λ (f g) #`(define #,(datum->syntax stx f) #,g)) fields gen-fields)
               #,@(map (λ (f g) #`(define (#,g) #,(datum->syntax stx f))) fields getters)
               #,@(map (λ (f s) #`(define (#,s val) (set! #,(datum->syntax stx f) val))) fields setters)))))]))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  ;; Examples
  (define-model foo (bar))
  (define value (make-foo 4))
  (define-values/invoke-unit value
    (import)
    (export foo))
  
  (check-pred unit? value)
  
  ;; Getters
  (check-equal? (get-bar) 4)
  
  ;; Setters
  (check-pred void? (set-bar! 5))
  (check-equal? (get-bar) 5))