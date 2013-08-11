#lang racket

(require (for-syntax syntax/parse racket/unit-exptime racket "private/utilities.rkt"))

;; Controller Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-controller)

;; controller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-controller stx)
  (syntax-parse stx
    [(_ model)
     (define-values (ident members defs syntaxe) (signature-members #'model #'model))
     (define cont-name (symbol-append (syntax->datum #'model) '-controller))
     (define members-with-ctx (map (curry datum->syntax stx) (map syntax->datum members)))
     #`(begin 
         (define-signature #,(datum->syntax stx cont-name) (#,@members-with-ctx))
         (define #,(datum->syntax stx (symbol-append 'make- cont-name))
           (λ (controller-model) 
             (compound-unit
               (import)
               (export C)
               (link [((M : model)) controller-model]
                     [((C : #,(datum->syntax stx cont-name))) 
                      (unit 
                        (import (prefix m: model))
                        (export #,(datum->syntax stx cont-name))
                        
                        #,@(for/list ([m (map syntax->datum members)])
                             #`(define #,(datum->syntax stx m) #,(datum->syntax stx (symbol-append 'm: m)))))
                      M])))))]))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit "model.rkt")
  
  ;; Examples
  (define-model foo (bar))
  (define model (make-foo 0))
  
  (define-controller foo)
  (define-values/invoke-unit (make-foo-controller model)
    (import)
    (export foo-controller))
  
  ;; getters
  (check-equal? (get-bar) 0)
  
  ;; setters
  (check-pred void? (set-bar! 1))
  (check-equal? (get-bar) 1))
