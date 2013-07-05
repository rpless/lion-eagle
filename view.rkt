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
;; - (button name text (action actionname))
(define-syntax (component stx)
  (syntax-case stx (frame textfield button vertical-panel
                          horizontal-panel message bind action)
    [(_ control (frame name title comp ...))
     #'(let () 
         (define name (new frame% [label title]))
         (component control name comp) ...
         name)]
    [(_ control parentname (horizontal-panel name comp ...))
     #`(begin 
         (define name (new horizontal-panel% [parent parentname]))
         (component control name comp)...)]
    [(_ control parentname (vertical-panel name comp ...))
     #`(begin 
         (define name (new vertical-panel% [parent parentname]))
         (component control name comp)...)]
    [(_ control parentname (textfield name (bind field model->textfield textfield->model)))
     #`(begin 
         (define name (new text-field% [parent parentname]
                           [label ""]
                           [callback (λ (self evt) 
                                       (when (eq? (send evt get-event-type) 'text-field-enter)
                                         (send control #,(datum->syntax stx (symbol-append 'set- (syntax->datum #'field)))
                                               (textfield->model (send self get-value)))))]))
         (send control #,(datum->syntax stx (symbol-append 'add-notifer: (syntax->datum #'field)))
               (λ (new-value) (send name set-value (model->textfield new-value)))))]
    [(_ control parentname (message name (bind field model->message)))
     #`(begin 
         (define name (new message% [parent parentname]
                           [label (model->message (send control #,(datum->syntax stx (symbol-append 'get- (syntax->datum #'field)))))]
                           [auto-resize #t]))
         (send control #,(datum->syntax stx (symbol-append 'add-notifer: (syntax->datum #'field)))
               (λ (new-value) (send name set-label (model->message new-value)))))]
    [(_ control parentname (button name text (action actionname)))
     #`(define name (new button% [parent parentname]
                         [label text]
                         [callback (λ (self evt) (when (eq? (send evt get-event-type) 'button)
                                                   (send control actionname)))]))]))