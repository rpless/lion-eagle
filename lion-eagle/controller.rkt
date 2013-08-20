#lang racket

(require (for-syntax syntax/parse racket/unit-exptime racket "private/utilities.rkt"))

;; Controller Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-controller)

;; controller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-controller stx)
  (define (->syntax val) (datum->syntax stx val))
  (define-syntax-class action
    #:description "a controller action"
    (pattern (define/action (action:id) exp ...)))
  
  (syntax-parse stx
    [(_ model a:action ...)
     (define-values (ident members defs syntaxe) (signature-members #'model #'model))
     (define member-names (map syntax->datum members))
     (define fields (remove-duplicates (map (curryr subsymbol (string-length "get-")) member-names)))
     (define cont-name (symbol-append (syntax->datum #'model) '-controller))
     (define-values (sym-getters sym-setters) (partition (curryr symbol-contains? 'get-) member-names))
     (define getters (map ->syntax sym-getters))
     (define setters (map ->syntax sym-setters))
     (define notifiers (map (compose ->syntax (curry symbol-append 'add-notifier:)) fields))
     (define action-ids (syntax->list #'(a.action ...)))
     (define action-impls (syntax->list #'((a.exp ...) ...)))
     
     #`(begin 
         (define-signature #,(->syntax cont-name) (#,@getters #,@setters #,@notifiers #,@action-ids))
         (define #,(->syntax (symbol-append 'make- cont-name))
           (λ (controller-model) 
             (define base-unit 
               (unit 
                 (import (prefix m: model))
                 (export #,(->syntax cont-name))
                 
                 #,@(make-fields-and-notifiers fields notifiers stx)
                 #,@(make-getters sym-getters stx)
                 #,@(make-setters sym-setters fields stx)
                 #,@(make-actions action-ids action-impls)))
             (compound-unit
               (import)
               (export C)
               (link [((M : model)) controller-model]
                     [((C : #,(->syntax cont-name))) base-unit M])))))]))

;; [Listof Symbol] [Listof Syntax] Syntax -> [Listof Syntax]
;; Create the syntax objects for notifiers and add-notifiers for each field.
(define-for-syntax (make-fields-and-notifiers fields add-notifiers stx)
  (for/list ([f fields][n add-notifiers])
    (define notifier-field (datum->syntax stx (symbol-append 'notifier: f)))
    #`(begin 
        (define #,notifier-field '())
        (define (#,n notifier) (set! #,notifier-field (cons notifier #,notifier-field))))))

;; [Listof Symbol] Syntax -> [Listof Syntax] 
;; create the syntax objects for all of the given getter names.
(define-for-syntax (make-getters getter-names stx)
  (for/list ([g getter-names])
    #`(define #,(datum->syntax stx g) #,(datum->syntax stx (symbol-append 'm: g)))))

;; [Listof Symbol] [Listof Symbol] Syntax -> [Listof Syntax]
;; create the syntax objects for the given setter names.
(define-for-syntax (make-setters setter-names fields stx)
  (for/list ([s setter-names][f fields])
    #`(define (#,(datum->syntax stx s) val)
        (#,(datum->syntax stx (symbol-append 'm: s)) val)
        (for ([n #,(datum->syntax stx (symbol-append 'notifier: f))])
          (n val)))))

(define-for-syntax (make-actions actions impls)
  (for/list ([id actions] [imps impls])
    #`(define (#,id) #,@imps)))

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
  (define output-string (open-output-string))
  (add-notifier:bar (λ (v) (display (~a "The new value is " v ".") output-string)))
  
  ;; getters
  (check-equal? (get-bar) 0)
  
  ;; setters
  (check-pred void? (set-bar! 1))
  (check-equal? (get-bar) 1)
  (check-equal? (get-output-string output-string) "The new value is 1."))
