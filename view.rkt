#lang racket

(require (for-syntax syntax/parse racket "utilities.rkt")
         syntax/parse)

;; View Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide textfield)

(define-syntax-class textfield
  #:description "UI Clause"
  (pattern (textfield name:id (bind field:id func:expr))))