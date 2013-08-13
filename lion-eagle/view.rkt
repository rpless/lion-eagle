#lang racket

(require (for-syntax racket "private/utilities.rkt")
         racket/gui/base)

;; View Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-view)

(define-syntax (define-view stx) 
  
  (define component 
    (λ (comp parentname)
      (syntax-case comp (bind textfield message)
        [(textfield name (bind field model->field field->model))
         #`(begin 
             (define callback 
               (λ (self evt)
                 (when (eq? (send evt get-event-type) 'text-field-enter)
                   (#,(datum->syntax stx (symbol-append 'set- (syntax->datum #'field) '!)) (field->model (send self get-value))))))
             (define initial-value (#,(datum->syntax stx (symbol-append 'get- (syntax->datum #'field)))))
             (define name (new text-field% [parent #,parentname][label ""][init-value (model->field initial-value)][callback callback]))
             (#,(datum->syntax stx (symbol-append 'add-notifier: (syntax->datum #'field)))
              (λ (new-value) (send name set-value (model->field new-value)))))]
        [(message name (bind field model->message))
         #`(begin 
             (define name (new message% [parent #,parentname]
                               [label (model->message (#,(datum->syntax stx (symbol-append 'get- (syntax->datum #'field)))))]
                               [auto-resize #t]))
             (#,(datum->syntax stx (symbol-append 'add-notifier: (syntax->datum #'field)))
              (λ (new-value) (send name set-label (model->message new-value)))))])))
  
  (syntax-case stx (frame)
    [(_ (frame parentname title comp ...))
     #`(let ()
         (define parentname (new frame% [label title]))
         #,@(map (curryr component #'parentname) (syntax->list #'(comp ...)))
         (send parentname show #t))]))

;; The component form supports the following types of UI components:
;; - (textfield name (bind field model->textfield textfield->model))
;;   The name is the id of the textfield, the field is the field in the model it is bound to
;;   The model->textfield is a function of [X -> String] and the textfield->model is a function
;;   of [String -> X]
;; - (button name text (action actionname))
#;
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