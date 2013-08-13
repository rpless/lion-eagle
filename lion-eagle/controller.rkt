#lang racket

(require (for-syntax syntax/parse racket/unit-exptime racket "private/utilities.rkt"))

;; Controller Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-controller)

;; controller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-controller stx)
  (define (->syntax val) (datum->syntax stx val))
  (syntax-parse stx
    [(_ model)
     (define-values (ident members defs syntaxe) (signature-members #'model #'model))
     (define member-names (map syntax->datum members))
     (define fields (remove-duplicates (map (curryr subsymbol (string-length "get-")) member-names)))
     (define cont-name (symbol-append (syntax->datum #'model) '-controller))
     (define-values (sym-getters sym-setters) (partition (curryr symbol-contains? 'get-) member-names))
     (define getters (map ->syntax sym-getters))
     (define setters (map ->syntax sym-setters))
     (define notifiers (map (compose ->syntax (curry symbol-append 'add-notifier:)) fields))
     #`(begin 
         (define-signature #,(->syntax cont-name) (#,@getters #,@setters #,@notifiers))
         (define #,(->syntax (symbol-append 'make- cont-name))
           (λ (controller-model) 
             (define base-unit 
               (unit 
                 (import (prefix m: model))
                 (export #,(->syntax cont-name))
                 
                 #,@(for/list ([f fields])
                      #`(begin 
                          (define #,(->syntax (symbol-append 'notifier: f)) '())
                          (define (#,(->syntax (symbol-append 'add-notifier: f)) notifier)
                            #,(let ([not-name (->syntax (symbol-append 'notifier: f))])
                                #`(set! #,not-name (cons notifier #,not-name))))))
                 
                 #,@(for/list ([g sym-getters])
                      #`(define #,(->syntax g) #,(->syntax (symbol-append 'm: g))))
                 #,@(for/list ([s sym-setters][f fields])
                      #`(define (#,(->syntax s) val)
                          (#,(->syntax (symbol-append 'm: s)) val)
                          (for ([n #,(->syntax (symbol-append 'notifier: f))])
                            (n val))))))
             (compound-unit
               (import)
               (export C)
               (link [((M : model)) controller-model]
                     [((C : #,(->syntax cont-name))) base-unit M])))))]))

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
