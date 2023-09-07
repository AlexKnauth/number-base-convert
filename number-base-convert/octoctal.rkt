#lang racket/base

(provide number->octoctal
         octoctal->number
         digits-octoctal
         digits-octoctal-url)

(require "number-string.rkt")
(module+ test
  (require rackunit))

;; Octoctal, aka OCC, aka base 64

;; RFC 4648 base64
;; printable ascii
;; mostly alphanumeric, but with + and / on the end
;; NOT url or filename safe
(define digits-octoctal
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;; RFC 4648 base64url
;; printable ascii
;; mostly alphanumeric, but with - and _ on the end
;; url and filename safe
(define digits-octoctal-url
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

;; number->octoctal : Number -> String
;; octoctal->number : String -> Number
(define (number->octoctal n #:digits [digits digits-octoctal])
  (number->string n
                  #:base 64
                  #:digits digits))

(define (octoctal->number s #:digits [digits digits-octoctal])
  (string->number s
                  #:base 64
                  #:digits digits))

(module+ test
  (define-check (check-number-octoctal n s)
    (check-equal? (number->octoctal n) s)
    (check-equal? (octoctal->number s) n))
  (define-check (check-number-octoctal-url n s)
    (check-equal? (number->octoctal n #:digits digits-octoctal-url) s)
    (check-equal? (octoctal->number s #:digits digits-octoctal-url) n))
  (check-number-octoctal 0 "A")
  (check-number-octoctal 25 "Z")
  (check-number-octoctal 26 "a")
  (check-number-octoctal 52 "0")
  (check-number-octoctal 61 "9")
  (check-number-octoctal 62 "+")
  (check-number-octoctal 63 "/")
  (check-number-octoctal 64 "BA")
  (check-number-octoctal 65 "BB")
  (check-number-octoctal 66 "BC")

  (check-number-octoctal-url 0 "A")
  (check-number-octoctal-url 25 "Z")
  (check-number-octoctal-url 26 "a")
  (check-number-octoctal-url 52 "0")
  (check-number-octoctal-url 61 "9")
  (check-number-octoctal-url 62 "-")
  (check-number-octoctal-url 63 "_")
  (check-number-octoctal-url 64 "BA")
  (check-number-octoctal-url 65 "BB")
  (check-number-octoctal-url 66 "BC")

  (check-exn #rx"number->string: conflicting sign and digits \"-\""
             (λ ()
               (number->octoctal -1 #:digits digits-octoctal-url)))
  (check-exn #rx"string->number: conflicting sign and digits \"\\+\""
             (λ ()
               (string->number "+"
                               #:sign (list "+" "" "-")
                               #:base 64
                               #:digits digits-octoctal)))
  (check-exn #rx"string->number: conflicting sign and digits \"-\""
             (λ ()
               (string->number "-"
                               #:sign (list "" "" "-")
                               #:base 64
                               #:digits digits-octoctal-url))))
