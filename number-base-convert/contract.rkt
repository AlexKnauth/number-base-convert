#lang racket/base

(provide around-out/c
         sign-out/c)

(require racket/contract/base)
(module+ test
  (require rackunit))

(define around-out/c
  (or/c string? (list/c string? string?)))

(define sign-out/c
  (or/c #f '+ '++ 'parens
        (list/c around-out/c around-out/c around-out/c)))

(module+ test
  (check-pred flat-contract? around-out/c)
  (check-pred flat-contract? sign-out/c))
