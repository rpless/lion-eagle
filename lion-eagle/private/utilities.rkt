#lang racket

;; Syntax Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide symbol-append extract-id)

;; Symbol ..+ -> Symbol
;; Take a wild guess as to what this function does.
(define (symbol-append s . rst)
  (string->symbol (apply string-append (map symbol->string (cons s rst)))))

(define (extract-id stx)
  (syntax-case stx ()
    [(id contract) #'id]
    [id #'id]))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  (check-equal? (symbol-append 'foo) 'foo)
  (check-equal? (symbol-append 'foo 'bar) 'foobar)
  (check-equal? (symbol-append 'foo 'bar 'baz) 'foobarbaz))