#lang racket

;; Syntax Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide symbol-append symbol-contains?)

;; Symbol ..+ -> Symbol
;; Take a wild guess as to what this function does.
(define (symbol-append s . rst)
  (string->symbol (apply string-append (map symbol->string (cons s rst)))))

;; Symbol Symbol -> Boolean
;; is the second symbol contained in the first symbol?
(define (symbol-contains? src pattern)
  (regexp-match? (symbol->string pattern) (symbol->string src)))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  ;; symbol-append
  (check-equal? (symbol-append 'foo) 'foo)
  (check-equal? (symbol-append 'foo 'bar) 'foobar)
  (check-equal? (symbol-append 'foo 'bar 'baz) 'foobarbaz)
  
  ;; symbol-contains?
  (check-true (symbol-contains? 'get-bar 'get.*))
  (check-false (symbol-contains? 'get-bar 'set.*)))