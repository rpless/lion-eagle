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
     (define member-names (map syntax->datum members))
     (define fields (remove-duplicates (map (curryr subsymbol (string-length "get-")) member-names)))
     (define cont-name (symbol-append (syntax->datum #'model) '-controller))
     (define-values (sym-getters sym-setters) (partition (curryr symbol-contains? 'get-) member-names))
     (define getters (map (curry datum->syntax stx) sym-getters))
     (define setters (map (curry datum->syntax stx) sym-setters))
     (define notifiers (map (compose (curry datum->syntax stx) (curry symbol-append 'add-notifier:)) fields))
     (define members-with-ctx (map (curry datum->syntax stx) member-names))
     #`(begin 
         (define-signature #,(datum->syntax stx cont-name) (#,@getters #,@setters #,@notifiers))
         (define #,(datum->syntax stx (symbol-append 'make- cont-name))
           (λ (controller-model) 
             (define base-unit 
               (unit 
                 (import (prefix m: model))
                 (export #,(datum->syntax stx cont-name))
                 
                 #,@(for/list ([f fields])
                      #`(begin 
                          (define #,(datum->syntax stx (symbol-append 'notifier: f)) '())
                          (define (#,(datum->syntax stx (symbol-append 'add-notifier: f)) notifier)
                          #,(let ([not-name (datum->syntax stx (symbol-append 'notifier: f))])
                              #`(set! #,not-name (cons notifier #,not-name))))))
                 
                 #,@(for/list ([g sym-getters])
                      #`(define #,(datum->syntax stx g) #,(datum->syntax stx (symbol-append 'm: g))))
                 #,@(for/list ([s sym-setters][f fields])
                      #`(define (#,(datum->syntax stx s) val)
                          (#,(datum->syntax stx (symbol-append 'm: s)) val)
                          (for ([n #,(datum->syntax stx (symbol-append 'notifier: f))])
                            (n val))))))
             (compound-unit
               (import)
               (export C)
               (link [((M : model)) controller-model]
                     [((C : #,(datum->syntax stx cont-name))) base-unit M])))))]))

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
  
  (add-notifier:bar (λ (v) (displayln (~a "The new value is " v "."))))
  
  ;; getters
  (check-equal? (get-bar) 0)
  
  ;; setters
  (check-pred void? (set-bar! 1))
  (check-equal? (get-bar) 1))
