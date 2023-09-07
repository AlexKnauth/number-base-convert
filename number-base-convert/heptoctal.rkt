#lang racket/base

(provide number->heptoctal
         heptoctal->number
         digits-heptoctal-py
         digits-heptoctal-php
         digits-heptoctal-go)

(require "number-string.rkt")
(module+ test
  (require rackunit))

;; Heptoctal, aka HET, aka base 56

;; alphanumeric
;; no 0, O, or o
;; no 1, I, or l, but does have i
;; sorted: numeric, then uppercase, then lowercase
;; default
(define digits-heptoctal-py
  "23456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz")

;; alphanumeric
;; no 0, O, or o
;; no 1, I, or l, but does have i
;; numeric, then lowercase, then uppercase
(define digits-heptoctal-php
  "23456789abcdefghijkmnpqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ")

;; alphanumeric
;; no O, D, Q, or o, but does have 0
;; no I or i, but does have 1 and l
(define digits-heptoctal-go
  "0123456789ABCEFGHJKLMNPRSTUVWXYZabcdefghjklmnpqrstuvwxyz")

;; alphanumeric
;; no 0, O, or o
;; no I, i, or l, but does have 1
;; sorted: numeric, then lowercase, then uppercase
(define digits-heptoctal-wikipedia
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz")

;; number->heptoctal : Number -> String
;; heptoctal->number : String -> Number
(define (number->heptoctal n #:digits [digits digits-heptoctal-py])
  (number->string n
                  #:base 56
                  #:digits digits))

(define (heptoctal->number s #:digits [digits digits-heptoctal-py])
  (string->number s
                  #:base 56
                  #:digits digits))

(module+ test
  (define-check (check-number-heptoctal n s)
    (check-equal? (number->heptoctal n) s)
    (check-equal? (heptoctal->number s) n))
  (check-number-heptoctal 0 "2")
  (check-number-heptoctal 1 "3")
  (check-number-heptoctal 7 "9")
  (check-number-heptoctal 8 "A")
  (check-number-heptoctal 31 "Z")
  (check-number-heptoctal 32 "a")
  (check-number-heptoctal 55 "z")
  (check-number-heptoctal 56 "32")
  (check-number-heptoctal 57 "33")
  (check-number-heptoctal 58 "34"))
