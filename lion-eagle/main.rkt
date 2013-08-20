#lang racket
(require (for-syntax syntax/parse racket "private/utilities.rkt")
         "view.rkt")

;; MVC Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide mvc)

(define-syntax (mvc stx)
  (syntax-parse stx 
    [(_ (controller-name controller) (model-name model) view)
     (define ->syntax (curry datum->syntax stx))
     #`(begin
         (define model-id model) 
         (define-values/invoke-unit
           (compound-unit
             (import)
             (export V)
             (link [((M : #,(->syntax #'model-name))) model-id C]
                   [((C : #,(->syntax #'controller-name))) (controller model-id) M]
                   [((V : view-factory^)) view C]))
           (import)
           (export view-factory^))
         (make-view))]))