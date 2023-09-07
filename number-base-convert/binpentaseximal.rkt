#lang racket/base

(provide number->binpentaseximal
         binpentaseximal->number
         digits-binpentaseximal)

(require "number-string.rkt")
(module+ test
  (require rackunit))

;; Binpentaseximal, aka BIP, aka base 62

;; alphanumeric
;; sorted: numeric, then uppercase, then lowercase
(define digits-binpentaseximal
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

;; number->bintetraseptimal : Number -> String
;; bintetraseptimal->number : String -> Number
(define (number->binpentaseximal n #:digits [digits digits-binpentaseximal])
  (number->string n
                  #:base 62
                  #:digits digits))

(define (binpentaseximal->number s #:digits [digits digits-binpentaseximal])
  (string->number s
                  #:base 62
                  #:digits digits))

(module+ test
  (define-check (check-number-binpentaseximal n s)
    (check-equal? (number->binpentaseximal n) s)
    (check-equal? (binpentaseximal->number s) n))
  (check-number-binpentaseximal 0 "0")
  (check-number-binpentaseximal 1 "1")
  (check-number-binpentaseximal 9 "9")
  (check-number-binpentaseximal 10 "A")
  (check-number-binpentaseximal 35 "Z")
  (check-number-binpentaseximal 36 "a")
  (check-number-binpentaseximal 61 "z")
  (check-number-binpentaseximal 62 "10")
  (check-number-binpentaseximal 63 "11")
  (check-number-binpentaseximal 64 "12"))
