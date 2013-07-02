#lang racket


;; Syntax Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (for-syntax symbol-append))

;; Symbol Symbol -> Symbol
;; Take a wild guess as to what this function does.
(define-for-syntax (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1)
                                 (symbol->string s2))))