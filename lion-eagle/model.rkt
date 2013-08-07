#lang racket

(require (for-syntax syntax/parse racket "private/utilities.rkt")
         racket/contract/base)

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide model)

;; Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (model stx)
  (syntax-parse stx
    [(_ (field-spec ...))
     (define ids (map extract-id (syntax->list #'(field-spec ...))))
     (define internal-ids (map (compose (curry datum->syntax stx) (curry symbol-append 'in:) syntax->datum) ids))
     (define getters (map (compose (curry symbol-append 'get-) syntax->datum) ids))
     (define setters (map (λ (i) (symbol-append 'set- (syntax->datum i) '!)) ids))
     (define contracts (map (case-lambda 
                              [(id contract) #'contract]
                              [(id) #'any/c])
                            (syntax->list #'(field-spec ...))))
     #`(let () 
         (define/contract hidden-model
           (class/c 
            (init #,@(map (λ (id contract) #`(#,id #,contract)) ids contracts))
            #,@(map (λ (id contract) #`(#,id (->m #,contract))) getters contracts)
            #,@(map (λ (id contract) #`(#,id (->m #,contract any/c))) setters contracts))
           (class object% 
             (super-new)
             (inspect #f)
             (init #,@(map values ids))
             #,@(map (λ (in i) #`(define #,in #,i)) internal-ids ids)
             
             #,@(map (λ (g in) #`(define/public (#,g) #,in)) getters internal-ids)
             #,@(map (λ (s in) #`(define/public (#,s val) (set! #,in val))) setters internal-ids)))
         
         (define/contract hidden-base-controller
           (class/c 
            #,@(map (λ (id contract) #`(#,id (->m #,contract))) getters contracts)
            #,@(map (λ (id contract) #`(#,id (->m #,contract any/c))) setters contracts))
           (class object% 
             (super-new)
             (inspect #f)
             (init model)
             (define in:model model)
             
             #,@(map (λ (g) #`(define/public (#,g) (send in:model #,g))) getters)
             #,@(map (λ (s) #`(define/public (#,s val) (send in:model #,s val))) setters)))
         (values hidden-model hidden-base-controller))]))

;; Tests 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  (define-values (foo foo-controller) (model ([bar number?] baz)))
  (define test (new foo [bar 0] [baz 0]))
  (define test-controller (make-object foo-controller test))
  
  ;; Model
  (check-equal? (send test get-bar) 0)
  (check-equal? (send test get-baz) 0)
  
  (send test set-bar! 1)
  (check-equal? (send test get-bar) 1)
  
  (send test set-baz! 2)
  (check-equal? (send test get-baz) 2)
  
  ;; Base Controller
  (check-equal? (send test-controller get-bar) 1)
  (check-equal? (send test-controller get-baz) 2)
  
  (send test-controller set-bar! 10)
  (check-equal? (send test-controller get-bar) 10)
  
  (send test-controller set-baz! "string")
  (check-equal? (send test-controller get-baz) "string"))