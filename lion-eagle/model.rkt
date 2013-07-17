#lang racket

(require (for-syntax syntax/parse racket "private/utilities.rkt")
         racket/contract/base)

;; Model Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-model)

;; define-model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-model <id> [<field-spec> ...] [arg ...])
;; <field-spec> = id
;;              | [id contract]
;; This will fail if the number of field-specs is not equal to the number of args.

(define-syntax (define-model stx)
  (syntax-parse stx
    [(_ name:id (field-spec ...))
     (define ids (map extract-id (syntax->list #'(field-spec ...))))
     (define internal-ids (map (compose (curry datum->syntax stx) (curry symbol-append 'in:) syntax->datum) ids))
     (define getters (map (compose (curry symbol-append 'get-) syntax->datum) ids))
     (define setters (map (λ (i) (symbol-append 'set- (syntax->datum i) '!)) ids))
     (define contracts (map (lambda (stx)
                              (syntax-case stx ()
                                [(id contract) #'contract]
                                [id #'any/c]))
                            (syntax->list #'(field-spec ...))))
     #`(define/contract name
         (class/c 
          (init #,@(map (λ (id contract) #`(#,id #,contract)) ids contracts))
          #,@(map (λ (id contract) #`(#,id (->m #,contract))) getters contracts)
          #,@(map (λ (id contract) #`(#,id (->m #,contract any/c))) setters contracts))
         (class object% 
           (super-new)
           (inspect #f)
           (init #,@(map values ids))
           #,@(map (λ (in i) #`(define #,in #,i)) internal-ids ids)
           #,@(map (λ (g in)#`(define/public (#,g) #,in)) getters internal-ids)
           #,@(map (λ (s in)#`(define/public (#,s val) (set! #,in val))) setters internal-ids)))]))

;; Tests 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  (define-model foo% ([bar number?] baz))
  (define test (make-object foo% 0 0))
  
  (check-equal? (send test get-bar) 0)
  (check-equal? (send test get-baz) 0)
  
  (send test set-bar! 1)
  (check-equal? (send test get-bar) 1)
  
    (send test set-baz! 2)
  (check-equal? (send test get-baz) 2))