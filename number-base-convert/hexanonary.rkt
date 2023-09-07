#lang racket/base

(provide number->hexanonary
         hexanonary->number
         digits-hexanonary)

(require "number-string.rkt")
(module+ test
  (require rackunit))

;; Hexanonary, aka HEN, aka base 54

;; alphanumeric
;; no 0, O, or o
;; no 1, I, L, i, or l
;; sorted: numeric, then uppercase, then lowercase
(define digits-hexanonary
  "23456789ABCDEFGHJKMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz")

;; number->hexanonary : Number -> String
;; hexanonary->number : String -> Number
(define (number->hexanonary n #:digits [digits digits-hexanonary])
  (number->string n
                  #:base 54
                  #:digits digits))

(define (hexanonary->number s #:digits [digits digits-hexanonary])
  (string->number s
                  #:base 54
                  #:digits digits))

(module+ test
  (define-check (check-number-hexanonary n s)
    (check-equal? (number->hexanonary n) s)
    (check-equal? (hexanonary->number s) n))
  (check-number-hexanonary 0 "2")
  (check-number-hexanonary 1 "3")
  (check-number-hexanonary 7 "9")
  (check-number-hexanonary 8 "A")
  (check-number-hexanonary 30 "Z")
  (check-number-hexanonary 31 "a")
  (check-number-hexanonary 53 "z")
  (check-number-hexanonary 54 "32")
  (check-number-hexanonary 55 "33")
  (check-number-hexanonary 56 "34")
  (check-number-hexanonary 107 "3z")
  (check-number-hexanonary 108 "42")
  (check-number-hexanonary 109 "43")
  (check-number-hexanonary 2861 "yz")
  (check-number-hexanonary 2915 "zz")
  (check-number-hexanonary 2916 "322")
  (check-number-hexanonary 2971 "333")
  (check-number-hexanonary 3027 "345")
  (check-number-hexanonary 5886 "432")
  (check-number-hexanonary 151577 "xyz")
  (check-number-hexanonary 157463 "zzz")
  (check-number-hexanonary 157464 "3222"))
