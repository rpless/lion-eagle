#lang racket

(require (for-syntax racket)
         "private/component.rkt" racket/gui/base)

;; View Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide view)

(define-syntax (view stx)   
  (syntax-case stx (frame)
    [(_ (frame parentname title comp ...))
     #`(lambda ()
         (define parentname (new frame% [label title]))
         #,@(map (curryr component #'parentname stx) (syntax->list #'(comp ...)))
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
    [(_ control parentname (button name text (action actionname)))
     #`(define name (new button% [parent parentname]
                         [label text]
                         [callback (λ (self evt) (when (eq? (send evt get-event-type) 'button)
                                                   (send control actionname)))]))]))