#lang racket

(require (for-syntax racket syntax/parse)
         "private/component.rkt" racket/gui/base)

;; View Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The view module allows for the creation of gui frames with a number of 
;; subcomponents.


(provide view view-factory^)


(define-signature view-factory^ (make-view))

(define-syntax (view stx)   
  (syntax-parse stx
    [(_ controller-name (frame parentname title comp ...))
     #`(unit 
         (import controller-name)
         (export view-factory^)
         (define (make-view)
           (define parentname (new frame% [label title]))
           #,@(map (curryr component #'parentname stx) (syntax->list #'(comp ...)))
           (send parentname show #t)))]))