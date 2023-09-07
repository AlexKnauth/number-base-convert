#lang racket/base

(provide number->bintetraseptimal
         bintetraseptimal->number
         digits-bintetraseptimal-btc
         digits-bintetraseptimal-flickr)

(require "number-string.rkt")
(module+ test
  (require rackunit))

;; Bintetraseptimal, aka BNT, aka base 58

;; alphanumenic
;; no 0 or O
;; no I or l, but does have 1
;; sorted: numeric, then uppercase, then lowercase
(define digits-bintetraseptimal-btc
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

;; alphanumenic
;; no 0 or O
;; no I or l, but does have 1
;; numeric, then lowercase, then uppercase
(define digits-bintetraseptimal-flickr
  "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ")

;; number->bintetraseptimal : Number -> String
;; bintetraseptimal->number : String -> Number
(define (number->bintetraseptimal n #:digits [digits digits-bintetraseptimal-btc])
  (number->string n
                  #:base 58
                  #:digits digits))

(define (bintetraseptimal->number s #:digits [digits digits-bintetraseptimal-btc])
  (string->number s
                  #:base 58
                  #:digits digits))

(module+ test
  (define-check (check-number-bintetraseptimal n s)
    (check-equal? (number->bintetraseptimal n) s)
    (check-equal? (bintetraseptimal->number s) n))
  (check-number-bintetraseptimal 0 "1")
  (check-number-bintetraseptimal 8 "9")
  (check-number-bintetraseptimal 9 "A")
  (check-number-bintetraseptimal 32 "Z")
  (check-number-bintetraseptimal 33 "a")
  (check-number-bintetraseptimal 57 "z")
  (check-number-bintetraseptimal 58 "21")
  (check-number-bintetraseptimal 59 "22")
  (check-number-bintetraseptimal 60 "23"))
