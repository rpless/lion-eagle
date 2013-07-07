#lang racket

;; Syntax Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide symbol-append extract-id)

;; Symbol Symbol -> Symbol
;; Take a wild guess as to what this function does.
(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1)
                                 (symbol->string s2))))


(define (extract-id stx)
  (syntax-case stx ()
    [(id contract) #'id]
    [id #'id]))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  (check-equal? (symbol-append 'foo 'bar) 'foobar))