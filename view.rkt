#lang racket

(require (for-syntax racket "utilities.rkt")
         racket/gui/base)

;; View Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide component)

;; The component form supports the following types of UI components:
;; - (textfield name (bind field model->textfield textfield->model))
;;   The name is the id of the textfield, the field is the field in the model it is bound to
;;   The model->textfield is a function of [X -> String] and the textfield->model is a function
;;   of [String -> X]
(define-syntax (component stx)
  (syntax-case stx (textfield button bind action)
    [(_ control parentname (textfield name (bind field model->textfield textfield->model)))
     #`(begin 
         (define name (new text-field% [label ""]
                           [parent parentname]
                           [callback (λ (self evt) 
                                       (when (eq? (send evt get-event-type) 'text-field-enter)
                                           (send control #,(datum->syntax stx (symbol-append 'set- (syntax->datum #'field)))
                                                 (textfield->model (send self get-value)))))]))
         (send control #,(datum->syntax stx (symbol-append 'add-notifer: (syntax->datum #'field)))
               (λ (new-value) (send name set-value (model->textfield new-value)))))]
    [(_ control parentname (button name text (action actionname)))
     #`(begin (define name (new button% [parent parentname]
                                    [label text]
                                    [callback (λ (self evt) (when (eq? (send evt get-event-type) 'button)
                                                              (send control actionname)))])))]))