#lang racket/base

(provide number->pentasuboptimal
         pentasuboptimal->number
         digits-pentasuboptimal)

(require "number-string.rkt")
(module+ test
  (require rackunit))

;; Pentasuboptimal, aka PES, aka base 85

;; RFC 1924
;; printable ascii
;; excluding "',./:[\]
(define digits-pentasuboptimal
  (string-append
   "0123456789"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "abcdefghijklmnopqrstuvwxyz"
   "!#$%&()*+-;<=>?@^_`{|}~"))

;; number->pentasuboptimal : Number -> String
;; pentasuboptimal->number : String -> Number
(define (number->pentasuboptimal n #:digits [digits digits-pentasuboptimal])
  (number->string n
                  #:base 85
                  #:digits digits))

(define (pentasuboptimal->number s #:digits [digits digits-pentasuboptimal])
  (string->number s
                  #:base 85
                  #:digits digits))

(module+ test
  (define-check (check-number-pentasuboptimal n s)
    (check-equal? (number->pentasuboptimal n) s)
    (check-equal? (pentasuboptimal->number s) n))
  (check-number-pentasuboptimal 0 "0")
  (check-number-pentasuboptimal 9 "9")
  (check-number-pentasuboptimal 10 "A")
  (check-number-pentasuboptimal 35 "Z")
  (check-number-pentasuboptimal 36 "a")
  (check-number-pentasuboptimal 61 "z")
  (check-number-pentasuboptimal 62 "!")
  (check-number-pentasuboptimal 69 "*")
  (check-number-pentasuboptimal 70 "+")
  (check-number-pentasuboptimal 71 "-")
  (check-number-pentasuboptimal 72 ";")
  (check-number-pentasuboptimal 77 "@")
  (check-number-pentasuboptimal 78 "^")
  (check-number-pentasuboptimal 79 "_")
  (check-number-pentasuboptimal 80 "`")
  (check-number-pentasuboptimal 81 "{")
  (check-number-pentasuboptimal 84 "~")
  (check-number-pentasuboptimal 85 "10")
  (check-number-pentasuboptimal 86 "11")
  (check-number-pentasuboptimal 87 "12")
  (check-number-pentasuboptimal (sub1 (expt 2 32)) "|NsC0")
  (check-number-pentasuboptimal (expt 2 32) "|NsC1")
  (check-number-pentasuboptimal 21932261930451111902915077091070067066
                                "4)+k&C#VzJ4br>0wv%Yp")
  (check-number-pentasuboptimal (sub1 (expt 2 128)) "=r54lj&NUUO~Hi%c2ym0")
  (check-number-pentasuboptimal (expt 2 128) "=r54lj&NUUO~Hi%c2ym1")

  (check-exn #rx"number->string: conflicting sign and digits \"-\""
             (λ ()
               (number->pentasuboptimal -1)))
  (check-exn #rx"string->number: conflicting sign and digits \"\\+\""
             (λ ()
               (string->number "+"
                               #:sign (list "+" "" "-")
                               #:base 85
                               #:digits digits-pentasuboptimal)))
  (check-exn #rx"string->number: conflicting sign and digits \"-\""
             (λ ()
               (string->number "-"
                               #:sign (list "" "" "-")
                               #:base 85
                               #:digits digits-pentasuboptimal))))
