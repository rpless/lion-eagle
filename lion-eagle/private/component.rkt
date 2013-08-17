#lang racket
(require (for-syntax racket "utilities.rkt")
         racket/gui/base)

;; Component Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module provides functions for creating components in the view.

(provide (for-syntax component))

;; Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (component comp parentname stx)
  (define ->syntax (curry datum->syntax stx))
  (syntax-case comp (bind action textfield message button)
    [(textfield name (bind field model->field field->model))
     (create-textfield #'name #'field parentname #'model->field #'field->model stx)]
    [(message name (bind field model->message))
     (create-message #'name #'field parentname #'model->message stx)]
    [(button name text (action id))
     (create-button #'name parentname #'text #'id)]))


;; Gui Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (create-button name parentname text action)
  #`(define name (new button% [parent #,parentname]
                      [label #,text]
                      [callback (λ (self evt)
                                  (when (eq? (send evt get-event-type) 'button)
                                    (#,action)))])))

(define-for-syntax (create-textfield name field parentname model->field field->model stx)
  (define ->syntax (curry datum->syntax stx))
  #`(begin 
      (define callback 
        (λ (self evt)
          (when (eq? (send evt get-event-type) 'text-field-enter)
            (#,(->syntax (symbol-append 'set- (syntax->datum field) '!)) (#,field->model (send self get-value))))))
      (define initial-value (#,(->syntax (symbol-append 'get- (syntax->datum field)))))
      (define #,name (new text-field% [parent #,parentname][label ""][init-value (#,model->field initial-value)][callback callback]))
      (#,(->syntax (symbol-append 'add-notifier: (syntax->datum field)))
       (λ (new-value) (send #,name set-value (#,model->field new-value))))))


(define-for-syntax (create-message name field parentname model->message stx)
  (define ->syntax (curry datum->syntax stx))
  #`(begin 
      (define #,name (new message% [parent #,parentname]
                          [label (#,model->message (#,(->syntax (symbol-append 'get- (syntax->datum field)))))]
                          [auto-resize #t]))
      (#,(->syntax (symbol-append 'add-notifier: (syntax->datum field)))
       (λ (new-value) (send #,name set-label (#,model->message new-value))))))