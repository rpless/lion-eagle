#lang racket

;; Syntax Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide symbol-append)

;; Symbol Symbol -> Symbol
;; Take a wild guess as to what this function does.
(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1)
                                 (symbol->string s2))))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  (check-equal? (symbol-append 'foo 'bar) 'foobar))