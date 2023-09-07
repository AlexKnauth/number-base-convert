#lang racket/base

(provide number->string
         string->number
         digits-extended
         digits-extended-up)

(require racket/match
         racket/math
         racket/string
         math/flonum
         math/number-theory
         (only-in srfi/13 string-index))
(module+ test
  (require racket/format
           rackunit
           syntax/parse/define))

(define digits-extended "0123456789abcdefghijklmnopqrstuvwxyz")
(define digits-extended-up
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

;; ---------------------------------------------------------

;; number->string : Number -> String
(module+ test
  (test-case "integers"
    (check-equal? (number->string 0) "0")
    (check-equal? (number->string 1) "1")
    (check-equal? (number->string 9) "9")
    (check-equal? (number->string 10) "10")
    (check-equal? (number->string 12) "12")
    (check-equal? (number->string 99) "99")
    (check-equal? (number->string 100) "100")
    (check-equal? (number->string 123) "123")
    (check-equal? (number->string 210) "210")
    (for ([_i (in-range 1000)])
      (define s (sub1 (* 2 (random 2))))
      (define n (random 1000000))
      (define i (* s n))
      (define b (+ 2 (random 34)))
      (check-equal? (number->string i #:base b)
                    (~r i #:base b))))

  (test-case "terminating decimals"
    (check-equal? (number->string 1.0) "1.0")
    (check-equal? (number->string 2.5) "2.5")
    (check-equal? (number->string 3.14159) "3.14159")
    (check-equal? (number->string #e3.14159) "3.14159")
    (check-equal? (number->string 3.14159265) "3.14159265")
    (check-equal? (number->string #e3.14159265) "3.14159265")
    (check-equal? (number->string #o14603.52 #:base 8) "14603.52")
    (check-equal? (number->string #e#o14603.52 #:base 8) "14603.52")
    (for ([_i (in-range 1000)])
      (define n (random 1000000))
      (define e (- (random 6)))
      (define b (+ 2 (random 34)))
      (define f (* n (expt b e)))
      (check-equal? (number->string f #:base b)
                    (~r f #:base b))))

  (test-case "repeating in binary"
    (check-equal? (number->string 1/3 #:base 2) "0._01_")
    (check-equal? (number->string 2/3 #:base 2) "0._10_")
    (check-equal? (number->string 1/5 #:base 2) "0._0011_")
    (check-equal? (number->string 2/5 #:base 2) "0._0110_")
    (check-equal? (number->string 3/5 #:base 2) "0._1001_")
    (check-equal? (number->string 4/5 #:base 2) "0._1100_")
    (check-equal? (number->string 1/6 #:base 2) "0.0_01_")
    (check-equal? (number->string 5/6 #:base 2) "0.1_10_")
    (check-equal? (number->string 1/7 #:base 2) "0._001_")
    (check-equal? (number->string 2/7 #:base 2) "0._010_")
    (check-equal? (number->string 3/7 #:base 2) "0._011_")
    (check-equal? (number->string 4/7 #:base 2) "0._100_")
    (check-equal? (number->string 5/7 #:base 2) "0._101_")
    (check-equal? (number->string 6/7 #:base 2) "0._110_")
    (check-equal? (number->string 1/9 #:base 2) "0._000111_")
    (check-equal? (number->string 2/9 #:base 2) "0._001110_")
    (check-equal? (number->string 4/9 #:base 2) "0._011100_")
    (check-equal? (number->string 5/9 #:base 2) "0._100011_")
    (check-equal? (number->string 7/9 #:base 2) "0._110001_")
    (check-equal? (number->string 8/9 #:base 2) "0._111000_")
    (check-equal? (number->string 1/10 #:base 2) "0.0_0011_")
    (check-equal? (number->string 3/10 #:base 2) "0.0_1001_")
    (check-equal? (number->string 7/10 #:base 2) "0.1_0110_")
    (check-equal? (number->string 9/10 #:base 2) "0.1_1100_")
    (check-equal? (number->string  1/11 #:base 2) "0._0001011101_")
    (check-equal? (number->string  2/11 #:base 2) "0._0010111010_")
    (check-equal? (number->string  3/11 #:base 2) "0._0100010111_")
    (check-equal? (number->string  4/11 #:base 2) "0._0101110100_")
    (check-equal? (number->string  5/11 #:base 2) "0._0111010001_")
    (check-equal? (number->string  6/11 #:base 2) "0._1000101110_")
    (check-equal? (number->string  7/11 #:base 2) "0._1010001011_")
    (check-equal? (number->string  8/11 #:base 2) "0._1011101000_")
    (check-equal? (number->string  9/11 #:base 2) "0._1101000101_")
    (check-equal? (number->string 10/11 #:base 2) "0._1110100010_")
    (check-equal? (number->string  1/12 #:base 2) "0.00_01_")
    (check-equal? (number->string  5/12 #:base 2) "0.01_10_")
    (check-equal? (number->string  7/12 #:base 2) "0.10_01_")
    (check-equal? (number->string 11/12 #:base 2) "0.11_10_"))
  (test-case "repeating in quaternary"
    (check-equal? (number->string 1/3 #:base 4) "0._1_")
    (check-equal? (number->string 2/3 #:base 4) "0._2_")
    (check-equal? (number->string 1/5 #:base 4) "0._03_")
    (check-equal? (number->string 2/5 #:base 4) "0._12_")
    (check-equal? (number->string 3/5 #:base 4) "0._21_")
    (check-equal? (number->string 4/5 #:base 4) "0._30_")
    (check-equal? (number->string 1/6 #:base 4) "0.0_2_")
    (check-equal? (number->string 5/6 #:base 4) "0.3_1_")
    (check-equal? (number->string 1/7 #:base 4) "0._021_")
    (check-equal? (number->string 2/7 #:base 4) "0._102_")
    (check-equal? (number->string 3/7 #:base 4) "0._123_")
    (check-equal? (number->string 4/7 #:base 4) "0._210_")
    (check-equal? (number->string 5/7 #:base 4) "0._231_")
    (check-equal? (number->string 6/7 #:base 4) "0._312_")
    (check-equal? (number->string 1/9 #:base 4) "0._013_")
    (check-equal? (number->string 2/9 #:base 4) "0._032_")
    (check-equal? (number->string 4/9 #:base 4) "0._130_")
    (check-equal? (number->string 5/9 #:base 4) "0._203_")
    (check-equal? (number->string 7/9 #:base 4) "0._301_")
    (check-equal? (number->string 8/9 #:base 4) "0._320_")
    (check-equal? (number->string 1/10 #:base 4) "0.0_12_")
    (check-equal? (number->string 3/10 #:base 4) "0.1_03_")
    (check-equal? (number->string 7/10 #:base 4) "0.2_30_")
    (check-equal? (number->string 9/10 #:base 4) "0.3_21_")
    (check-equal? (number->string  1/11 #:base 4) "0._01131_")
    (check-equal? (number->string  2/11 #:base 4) "0._02322_")
    (check-equal? (number->string  3/11 #:base 4) "0._10113_")
    (check-equal? (number->string  4/11 #:base 4) "0._11310_")
    (check-equal? (number->string  5/11 #:base 4) "0._13101_")
    (check-equal? (number->string  6/11 #:base 4) "0._20232_")
    (check-equal? (number->string  7/11 #:base 4) "0._22023_")
    (check-equal? (number->string  8/11 #:base 4) "0._23220_")
    (check-equal? (number->string  9/11 #:base 4) "0._31011_")
    (check-equal? (number->string 10/11 #:base 4) "0._32202_")
    (check-equal? (number->string  1/12 #:base 4) "0.0_1_")
    (check-equal? (number->string  5/12 #:base 4) "0.1_2_")
    (check-equal? (number->string  7/12 #:base 4) "0.2_1_")
    (check-equal? (number->string 11/12 #:base 4) "0.3_2_"))
  (test-case "repeating in seximal"
    (check-equal? (number->string 1/5 #:base 6) "0._1_")
    (check-equal? (number->string 2/5 #:base 6) "0._2_")
    (check-equal? (number->string 3/5 #:base 6) "0._3_")
    (check-equal? (number->string 4/5 #:base 6) "0._4_")
    (check-equal? (number->string 1/7 #:base 6) "0._05_")
    (check-equal? (number->string 2/7 #:base 6) "0._14_")
    (check-equal? (number->string 3/7 #:base 6) "0._23_")
    (check-equal? (number->string 4/7 #:base 6) "0._32_")
    (check-equal? (number->string 5/7 #:base 6) "0._41_")
    (check-equal? (number->string 6/7 #:base 6) "0._50_")
    (check-equal? (number->string 1/10 #:base 6) "0.0_3_")
    (check-equal? (number->string 3/10 #:base 6) "0.1_4_")
    (check-equal? (number->string 7/10 #:base 6) "0.4_1_")
    (check-equal? (number->string 9/10 #:base 6) "0.5_2_")
    (check-equal? (number->string  1/11 #:base 6) "0._0313452421_")
    (check-equal? (number->string  2/11 #:base 6) "0._1031345242_")
    (check-equal? (number->string  3/11 #:base 6) "0._1345242103_")
    (check-equal? (number->string  4/11 #:base 6) "0._2103134524_")
    (check-equal? (number->string  5/11 #:base 6) "0._2421031345_")
    (check-equal? (number->string  6/11 #:base 6) "0._3134524210_")
    (check-equal? (number->string  7/11 #:base 6) "0._3452421031_")
    (check-equal? (number->string  8/11 #:base 6) "0._4210313452_")
    (check-equal? (number->string  9/11 #:base 6) "0._4524210313_")
    (check-equal? (number->string 10/11 #:base 6) "0._5242103134_")
    (check-equal? (number->string  7/22 #:base 6) "0.1_5242103134_"))
  (test-case "repeating in decimal"
    (check-equal? (number->string 1/3) "0._3_")
    (check-equal? (number->string 2/3) "0._6_")
    (check-equal? (number->string 1/6) "0.1_6_")
    (check-equal? (number->string 5/6) "0.8_3_")
    (check-equal? (number->string 1/7) "0._142857_")
    (check-equal? (number->string 2/7) "0._285714_")
    (check-equal? (number->string 3/7) "0._428571_")
    (check-equal? (number->string 4/7) "0._571428_")
    (check-equal? (number->string 5/7) "0._714285_")
    (check-equal? (number->string 6/7) "0._857142_")
    (check-equal? (number->string 1/9) "0._1_")
    (check-equal? (number->string 2/9) "0._2_")
    (check-equal? (number->string 4/9) "0._4_")
    (check-equal? (number->string 5/9) "0._5_")
    (check-equal? (number->string 7/9) "0._7_")
    (check-equal? (number->string 8/9) "0._8_")
    (check-equal? (number->string  1/11) "0._09_")
    (check-equal? (number->string  2/11) "0._18_")
    (check-equal? (number->string  3/11) "0._27_")
    (check-equal? (number->string  4/11) "0._36_")
    (check-equal? (number->string  5/11) "0._45_")
    (check-equal? (number->string  6/11) "0._54_")
    (check-equal? (number->string  7/11) "0._63_")
    (check-equal? (number->string  8/11) "0._72_")
    (check-equal? (number->string  9/11) "0._81_")
    (check-equal? (number->string 10/11) "0._90_")
    (check-equal? (number->string  1/12) "0.08_3_")
    (check-equal? (number->string  5/12) "0.41_6_")
    (check-equal? (number->string  7/12) "0.58_3_")
    (check-equal? (number->string 11/12) "0.91_6_"))
  (test-case "repeating in dozenal"
    (check-equal? (number->string 1/5 #:base 12) "0._2497_")
    (check-equal? (number->string 2/5 #:base 12) "0._4972_")
    (check-equal? (number->string 3/5 #:base 12) "0._7249_")
    (check-equal? (number->string 4/5 #:base 12) "0._9724_")
    (check-equal? (number->string 1/7 #:base 12) "0._186a35_")
    (check-equal? (number->string 2/7 #:base 12) "0._35186a_")
    (check-equal? (number->string 3/7 #:base 12) "0._5186a3_")
    (check-equal? (number->string 4/7 #:base 12) "0._6a3518_")
    (check-equal? (number->string 5/7 #:base 12) "0._86a351_")
    (check-equal? (number->string 6/7 #:base 12) "0._a35186_")))

(define (number->string n
                        #:base [base 10]
                        #:sign [sign #f]
                        #:digits [digits digits-extended]
                        #:point [point #f]
                        #:repeat [repeat #f]
                        #:etc [etc #f]
                        #:notation [notation 'positional]
                        #:exponent [exponent #f]
                        #:exponent-sign [exponent-sign #f]
                        #:exponent-base [exponent-base base])
  (number-repr->string
   (number->number-repr n
                        #:base base
                        #:notation notation
                        #:exponent-base exponent-base)
   #:sign sign
   #:digits digits
   #:point point
   #:repeat repeat
   #:etc etc
   #:exponent exponent
   #:exponent-sign exponent-sign))

;; string->number : String -> (U #f Number)
(module+ test
  (check-equal? (string->number "0") 0)
  (test-case "integers"
    (check-equal? (string->number "0") 0)
    (check-equal? (string->number "1") 1)
    (check-equal? (string->number "9") 9)
    (check-equal? (string->number "10") 10)
    (check-equal? (string->number "12") 12)
    (check-equal? (string->number "99") 99)
    (check-equal? (string->number "100") 100)
    (check-equal? (string->number "123") 123)
    (check-equal? (string->number "210") 210)
    (for ([_i (in-range 1000)])
      (define s (sub1 (* 2 (random 2))))
      (define n (random 1000000))
      (define i (* s n))
      (define b (+ 2 (random 34)))
      (check-equal? (string->number (number->string i #:base b) #:base b)
                    i)))
  (test-case "pi"
    (check-equal? (exact->inexact (string->number (number->string pi))) pi)))

(define (string->number s
                        #:base [base 10]
                        #:sign [sign #f]
                        #:digits [digits digits-extended]
                        #:point [point #f]
                        #:repeat [repeat #f]
                        #:etc [etc #f]
                        #:exponent [exponent #f]
                        #:exponent-sign [exponent-sign #f]
                        #:exponent-base [exponent-base base]
                        #:notation [_notation #f])
  (define nr
    (string->number-repr s
                         #:base base
                         #:sign sign
                         #:digits digits
                         #:point point
                         #:repeat repeat
                         #:etc etc
                         #:exponent exponent
                         #:exponent-sign exponent-sign
                         #:exponent-base exponent-base))
  (and nr (number-repr->number nr)))

(module+ test
  (define-syntax-parse-rule (check-number-string n s kwargs ...)
    (begin
      (check-equal? (number->string n kwargs ...) s)
      (check-= (string->number s kwargs ...) n 0)))
  (check-number-string 12345 "12345")
  (check-number-string 123.456 "123.456")
  (check-number-string 123.456 "123,456" #:point ",")
  (check-number-string 17 "17")
  (check-number-string 0 "0")
  (check-number-string -42 "-42")
  (check-number-string 17 "+17" #:sign '+)
  (check-number-string 0 "0" #:sign '+)
  (check-number-string -42 "-42" #:sign '+)
  (check-number-string 17 "+17" #:sign '++)
  (check-number-string 0 "+0" #:sign '++)
  (check-number-string -42 "-42" #:sign '++)
  (check-number-string 17 "17" #:sign 'parens)
  (check-number-string 0 "0" #:sign 'parens)
  (check-number-string -42 "(42)" #:sign 'parens)
  (let ([sign-table '(("" " up") "an even " ("" " down"))])
    (check-number-string 17 "17 up" #:sign sign-table)
    (check-number-string 0 "an even 0" #:sign sign-table)
    (check-number-string -42 "42 down" #:sign sign-table))
  (check-number-string 100 "202" #:base 7)
  (check-number-string 102 "204" #:base 7)
  (check-number-string 4.5 "100.1" #:base 2)
  (check-number-string 3735928559 "deadbeef" #:base 16)
  (check-number-string 3735928559 "DEADBEEF" #:base 16 #:digits digits-extended-up)
  (check-number-string (+ 102 1/7 2/49 3/343) "204.123" #:base 7)
  (check-number-string (+ 3892940 247/1296) "2.152345121051*6^+8"
                       #:base 6
                       #:notation 'exponential 
                       #:exponent "*6^"
                       #:exponent-base 10)
  (check-number-string 12345 "1.2345e+4" #:notation 'exponential)
  (check-number-string 1000 "1e+3" #:notation 'exponential)
  (check-number-string 0.9876 "9.876e-1" #:notation 'exponential)
  (check-number-string 100 "1.1001*2^+6" #:notation 'exponential #:base 2
                       #:exponent "*2^"
                       #:exponent-base 10)
  (check-number-string 1234 "1.234E+3" #:notation 'exponential
                       #:exponent "E"))

(module+ test
  (define-syntax-parse-rule
    (check-number-~r n s [kwargs1 ...] [kwargs2 ...] [kwargs3 ...])
    (begin
      (check-equal? (~r n kwargs1 ... kwargs2 ...) s)
      (check-= (string->number s kwargs1 ... kwargs3 ...) n 0)))
  (check-number-~r (+ 3892940 247/1296) "2.152345121051*6^+08"
                   [#:base 6 #:notation 'exponential]
                   [#:precision 20]
                   [#:exponent "*6^" #:exponent-base 10])
  (check-number-~r 12345 "1.2345e+04" [#:notation 'exponential] [] [])
  (check-number-~r 1000 "1e+03" [#:notation 'exponential] [] [])
  (check-number-~r 0.9876 "9.876e-01" [#:notation 'exponential] [] [])
  (check-number-~r 100 "1.1001*2^+06" [#:notation 'exponential #:base 2]
                   []
                   [#:exponent "*2^" #:exponent-base 10])
  (check-number-~r 1234 "1.234E+03" [#:notation 'exponential]
                   [#:format-exponent "E"]
                   [#:exponent "E"]))

;; ---------------------------------------------------------

;; A Sign is one of:
;;  - 0
;;  - 1
;;  - -1

;; number-repr:
;;   sign: Sign
;;   big: (Vectorof Natural)
;;   little: (Vectorof Natural)
;;   repeat: (U #false (Vectorof Natural))
;;   base: Natural
;;   exponent-sign: Sign
;;   exponent-big: (Vectorof Natural)
;;   exponent-base: Natural
(struct number-repr
  [sign big little repeat base exponent-sign exponent-big exponent-base]
  #:transparent)

;; number-repr->number : NumberRepr -> Number
(module+ test
  (test-case "wholes"
    (check-equal? (number-repr->number (number-repr 1 #(1) #() #() 6 0 #() 6)) 1)
    (check-equal? (number-repr->number (number-repr 1 #(5) #() #() 6 0 #() 6)) 5)
    (check-equal? (number-repr->number (number-repr 1 #(0 1) #() #() 6 0 #() 6)) 6)
    (check-equal? (number-repr->number (number-repr 1 #(5 1) #() #() 6 0 #() 6)) 11)
    (check-equal? (number-repr->number (number-repr 1 #(5 5) #() #() 6 0 #() 6)) 35)
    (check-equal? (number-repr->number (number-repr 1 #(0 0 1) #() #() 6 0 #() 6))
                  36))
  (test-case "halves"
    (check-equal? (number-repr->number (number-repr 1 #() #(3) #() 6 0 #() 6)) 1/2))
  (test-case "thirds"
    (check-equal? (number-repr->number (number-repr 1 #() #(2) #() 6 0 #() 6)) 1/3)
    (check-equal? (number-repr->number (number-repr 1 #() #(4) #() 6 0 #() 6)) 2/3))
  (test-case "fourths"
    (check-equal? (number-repr->number (number-repr 1 #() #(1 3) #() 6 0 #() 6))
                  1/4)
    (check-equal? (number-repr->number (number-repr 1 #() #(4 3) #() 6 0 #() 6))
                  3/4))
  (test-case "fifths"
    (check-equal? (number-repr->number (number-repr 1 #() #() #(1) 6 0 #() 6)) 1/5)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(2) 6 0 #() 6)) 2/5)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(3) 6 0 #() 6)) 3/5)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(4) 6 0 #() 6)) 4/5))
  (test-case "sixths"
    (check-equal? (number-repr->number (number-repr 1 #() #(1) #() 6 0 #() 6)) 1/6)
    (check-equal? (number-repr->number (number-repr 1 #() #(5) #() 6 0 #() 6)) 5/6))
  (test-case "sevenths"
    (check-equal? (number-repr->number (number-repr 1 #() #() #(0 5) 6 0 #() 6)) 1/7)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(1 4) 6 0 #() 6)) 2/7)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(2 3) 6 0 #() 6)) 3/7)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(3 2) 6 0 #() 6)) 4/7)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(4 1) 6 0 #() 6)) 5/7)
    (check-equal? (number-repr->number (number-repr 1 #() #() #(5 0) 6 0 #() 6)) 6/7)
    (void))
  (test-case "tenths"
    (check-equal? (number-repr->number (number-repr 1 #() #(0) #(3) 6 0 #() 6)) 1/10)
    (check-equal? (number-repr->number (number-repr 1 #() #(1) #(4) 6 0 #() 6)) 3/10)
    (check-equal? (number-repr->number (number-repr 1 #() #(4) #(1) 6 0 #() 6)) 7/10)
    (check-equal? (number-repr->number (number-repr 1 #() #(5) #(2) 6 0 #() 6)) 9/10)
    (void))
  (test-case "elevenths"
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(0 3 1 3 4 5 2 4 2 1) 6 0 #() 6))
     1/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(1 0 3 1 3 4 5 2 4 2) 6 0 #() 6))
     2/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(1 3 4 5 2 4 2 1 0 3) 6 0 #() 6))
     3/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(2 1 0 3 1 3 4 5 2 4) 6 0 #() 6))
     4/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(2 4 2 1 0 3 1 3 4 5) 6 0 #() 6))
     5/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(3 1 3 4 5 2 4 2 1 0) 6 0 #() 6))
     6/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(3 4 5 2 4 2 1 0 3 1) 6 0 #() 6))
     7/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(4 2 1 0 3 1 3 4 5 2) 6 0 #() 6))
     8/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(4 5 2 4 2 1 0 3 1 3) 6 0 #() 6))
     9/11)
    (check-equal?
     (number-repr->number (number-repr 1 #() #() #(5 2 4 2 1 0 3 1 3 4) 6 0 #() 6))
     10/11)))

(define (number-repr->number nr)
  (match-define (number-repr sign
                             big
                             little
                             repeat
                             base
                             exponent-sign
                             exponent-big
                             exponent-base)
    nr)
  (* sign
     (+ (big->number big base)
        (for/sum ([l (in-vector little)]
                  [i (in-naturals 1)])
          (* l (expt base (- i))))
        (* (expt base (- (vector-length little)))
           (repeat-fraction repeat base)))
     (expt base (* exponent-sign (big->number exponent-big exponent-base)))))

;; big->number : (Vectorof Natural) Natural
(module+ test
  (check-equal? (big->number #(1) 6) 1)
  (check-equal? (big->number #(5) 6) 5)
  (check-equal? (big->number #(0 1) 6) 6)
  (check-equal? (big->number #(5 1) 6) 11)
  (check-equal? (big->number #(5 5) 6) 35)
  (check-equal? (big->number #(0 0 1) 6) 36))

(define (big->number big base)
  (for/sum ([b (in-vector big)]
            [i (in-naturals)])
    (* b (expt base i))))

;; repeat-fraction : (U #f (Vectorof Natural)) Natural
(module+ test
  (check-equal? (repeat-fraction #(0) 6) 0)
  (check-equal? (repeat-fraction #(1) 6) 1/5)
  (check-equal? (repeat-fraction #(4) 6) 4/5)
  (check-equal? (repeat-fraction #(5) 6) 1))
(define (repeat-fraction repeat base)
  (cond
    [(not repeat) 0]
    [(zero? (vector-length repeat)) 0]
    [else
     (define R (vector-length repeat))
     (/ (for/sum ([r (in-vector repeat)]
                  [i (in-naturals)])
          (* r (expt base (- R i 1))))
        (sub1 (expt base R)))]))

;; exact-rationalize/ulp : Real -> ExactRational
(module+ test
  (check-equal? (exact-rationalize/ulp 0.1) #e0.1)
  (check-equal? (exact-rationalize/ulp 3.1) #e3.1)
  (check-equal? (exact-rationalize/ulp 3.14) #e3.14)
  (check-equal? (exact-rationalize/ulp 3.142) #e3.142)
  (check-equal? (exact-rationalize/ulp 3.1416) #e3.1416)
  (check-equal? (exact-rationalize/ulp 3.14159) #e3.14159)
  (check-equal? (exact-rationalize/ulp 3.141593) #e3.141593)
  (check-equal? (exact-rationalize/ulp 3.1415927) #e3.1415927)
  (check-equal? (exact-rationalize/ulp 3.14159265) #e3.14159265)
  (for ([_i (in-range 1000)])
    (define n (random))
    (check-equal? (fl (exact-rationalize/ulp n)) n))
  (for ([_i (in-range 1000)])
    (define i (random 10000000))
    (define n (* #e0.0000001 i))
    (check-equal? (exact-rationalize/ulp (fl n)) n)))

(define (exact-rationalize/ulp n)
  (cond
    [(exact? n) n]
    [(flonum? n)
     (define tolerance (* 1/2 (inexact->exact (flulp n))))
     (rationalize (inexact->exact n) tolerance)]
    [else (error 'exact-rationalize/ulp "bad ~v" n)]))

;; number->number-repr : Number #:base Natural #:exponent-base Natural
(module+ test
  (check-equal? (number->number-repr 0) (number-repr 0 #() #() #() 10 0 #() 10))
  (check-equal? (number->number-repr 9) (number-repr 1 #(9) #() #() 10 0 #() 10))
  (check-equal? (number->number-repr 10) (number-repr 1 #(0 1) #() #() 10 0 #() 10))
  (check-equal? (number->number-repr 99) (number-repr 1 #(9 9) #() #() 10 0 #() 10))
  (check-equal? (number->number-repr 100)
                (number-repr 1 #(0 0 1) #() #() 10 0 #() 10))
  (check-equal? (number->number-repr #e1.2345678)
                (number-repr 1 #(1) #(2 3 4 5 6 7 8) #() 10 0 #() 10))
  (check-equal? (number->number-repr #e#o14603.52 #:base 8)
                (number-repr 1 #(3 0 6 4 1) #(5 2) #() 8 0 #() 8)))

(define (number->number-repr n
                             #:base [base 10]
                             #:notation [notation 'positional]
                             #:exponent-base [exponent-base base])
  (define nttn (if (procedure? notation) (notation n) notation))
  (define szm1 (exponent n #:base base))
  (define exp (match nttn ['positional 0] ['exponential szm1]))
  (define EB (big-size exp #:base exponent-base))
  (define sign (sgn n))
  (define mag (* (exact-rationalize/ulp (abs n)) (expt base (- exp))))
  ;; MUTABLE
  (define big (exact-truncate mag))
  (define B (big-size big #:base base))
  ;; TODO: handle `notation` options: 'positional, 'exponential, and function
  (define frac (- mag big))
  (match-define (list i1 repeat i2 little)
    (find-repeat frac #:base base #:ulp (flulp (fl n))))
  ;; TODO: handle non-integer `repeat` as `...` on the end
  (define R (if (zero? repeat) 0 (- i2 i1)))
  (define L
    (cond
      [(and (zero? R) (inexact? n)) (max 1 (- 0 i2))]
      [(not (integer? repeat)) (+ R (- 0 i2))]
      [else (- 0 i2)]))
  (when (negative? B) (error 'number->number-repr "bad B: ~v" B))
  (when (negative? L) (error 'number->number-repr "bad L: ~v" L))
  (when (negative? R) (error 'number->number-repr "bad R: ~v" R))
  (number-repr
   sign
   (for/vector #:length B ([_i (in-range B)])
     (define-values [q r] (quotient/remainder big base))
     (set! big q)
     r)
   (for/vector #:length L ([_i (in-range L)])
     (define df (* base frac))
     (define d (exact-truncate df))
     (define f (- df d))
     (set! frac f)
     d)
   (and
    (integer? repeat)
    (for/vector #:length R ([_i (in-range R)])
      (define df (* base frac))
      (define d (exact-truncate df))
      (define f (- df d))
      (set! frac f)
      d))
   base
   (sgn exp)
   (for/vector #:length EB ([_i (in-range EB)])
     (define-values [q r] (quotient/remainder (abs exp) exponent-base))
     (set! exp q)
     r)
   exponent-base))

;; exponent : Number #:base Natural
(module+ test
  (check-equal? (exponent 0 #:base 10) 0)
  (check-equal? (exponent 1 #:base 10) 0)
  (check-equal? (exponent 9 #:base 10) 0)
  (check-equal? (exponent -1 #:base 10) 0)
  (check-equal? (exponent -9 #:base 10) 0)
  (check-equal? (exponent 10 #:base 10) 1)
  (check-equal? (exponent 99 #:base 10) 1)
  (check-equal? (exponent -10 #:base 10) 1)
  (check-equal? (exponent -99 #:base 10) 1)
  (check-equal? (exponent 100 #:base 10) 2)
  (check-equal? (exponent 999 #:base 10) 2)
  (check-equal? (exponent -100 #:base 10) 2)
  (check-equal? (exponent -999 #:base 10) 2)
  (check-equal? (exponent 1000 #:base 10) 3)
  (check-equal? (exponent -1000 #:base 10) 3)
  (check-equal? (exponent 0.9 #:base 10) -1)
  (check-equal? (exponent 0.1 #:base 10) -1)
  (check-equal? (exponent 0.09 #:base 10) -2)
  (check-equal? (exponent 0.01 #:base 10) -2)
  (check-equal? (exponent 0.009 #:base 10) -3)
  (check-equal? (exponent 0.001 #:base 10) -3)
  (check-equal? (exponent 0 #:base 6) 0)
  (check-equal? (exponent 1 #:base 6) 0)
  (check-equal? (exponent 5 #:base 6) 0)
  (check-equal? (exponent 6 #:base 6) 1)
  (check-equal? (exponent 35 #:base 6) 1)
  (check-equal? (exponent 36 #:base 6) 2)
  (check-equal? (exponent 215 #:base 6) 2)
  (check-equal? (exponent 216 #:base 6) 3)
  (check-equal? (exponent 5/6 #:base 6) -1)
  (check-equal? (exponent 1/6 #:base 6) -1)
  (check-equal? (exponent 1/7 #:base 6) -2)
  (check-equal? (exponent 1/36 #:base 6) -2)
  (check-equal? (exponent 1/37 #:base 6) -3)
  (check-equal? (exponent 1/216 #:base 6) -3)
  (check-equal? (exponent 0 #:base 19) 0))

(define (exponent n #:base base)
  (cond
    [(infinite? n) +inf.0]
    [(zero? n) 0]
    [else (exact-floor (fllogb (fl base) (flabs (fl n))))]))

;; big-size : Number #:base Natural
(module+ test
  (check-equal? (big-size 0 #:base 6) 0)
  (check-equal? (big-size 1 #:base 6) 1)
  (check-equal? (big-size 5 #:base 6) 1)
  (check-equal? (big-size 6 #:base 6) 2)
  (check-equal? (big-size 35 #:base 6) 2)
  (check-equal? (big-size 36 #:base 6) 3)
  (check-equal? (big-size 0 #:base 19) 0)
  (check-equal? (big-size 999 #:base 10) 3)
  (check-equal? (big-size 1000 #:base 10) 4))

(define (big-size n #:base base)
  (cond
    [(zero? n) 0]
    [else (add1 (exponent n #:base base))]))

;; find-repeat : ExactRational #:base Natural #:ulp Real -> (list Integer Integer)
;; A repeating "decimal" can be expressed as:
;;  n1             n2            n1*(b^de2 - 1) + n2   n
;; ----- + ------------------- = ------------------- = -
;; b^de1   b^de1 * (b^de2 - 1)   b^de1 * (b^de2 - 1)   d
;; To find de1, find the number of factors of b in d.
;; To find de2, try bigger numbers until it's a multiple.
;; Produces (list (- (+ de1 de2)) n2 (- de1) n1)
(module+ test
  (check-equal? (find-repeat 1 #:base 2) (list -1 1 0 0))
  (check-equal? (find-repeat 1/2 #:base 2) (list -2 0 -1 1))
  (check-equal? (find-repeat 1/3 #:base 2) (list -2 1 0 0))
  (check-equal? (find-repeat 2/3 #:base 2) (list -2 2 0 0))
  (check-equal? (find-repeat 1/4 #:base 2) (list -3 0 -2 1))
  (check-equal? (find-repeat 3/4 #:base 2) (list -3 0 -2 3))
  (check-equal? (find-repeat 1/6 #:base 2) (list -3 1 -1 0))
  (check-equal? (find-repeat 5/6 #:base 2) (list -3 2 -1 1))
  (check-equal? (find-repeat 1/7 #:base 2) (list -3 1 0 0))
  (check-equal? (find-repeat 2/7 #:base 2) (list -3 2 0 0))
  (check-equal? (find-repeat 3/7 #:base 2) (list -3 3 0 0))
  (check-equal? (find-repeat 4/7 #:base 2) (list -3 4 0 0))
  (check-equal? (find-repeat 5/7 #:base 2) (list -3 5 0 0))
  (check-equal? (find-repeat 6/7 #:base 2) (list -3 6 0 0))
  (check-equal? (find-repeat 1/8 #:base 2) (list -4 0 -3 1))
  (check-equal? (find-repeat 3/8 #:base 2) (list -4 0 -3 3))
  (check-equal? (find-repeat 5/8 #:base 2) (list -4 0 -3 5))
  (check-equal? (find-repeat 7/8 #:base 2) (list -4 0 -3 7))
  (check-equal? (find-repeat 7/12 #:base 2) (list -4 1 -2 2))
  (check-equal? (find-repeat #e#o0.52 #:base 8) (list -3 0 -2 #o52))
  (check-equal?
   (find-repeat
    (+ (/ 10456 (expt 10 5))
       (/ 285714 (* (expt 10 5) (sub1 (expt 10 6)))))
    #:base 10)
   (list -11 285714 -5 10456))
  (check-equal?
   (find-repeat
    (+ (/ 1398572 (expt 17 5))
       (/ 3129 (* (expt 17 5) (sub1 (expt 17 3)))))
    #:base 17)
   (list -8 3129 -5 1398572))
  (check-equal? (find-repeat 7/22 #:base 6) (list -11 54969250 -1 1)))

(define (find-repeat a #:base b #:ulp [ulp (flulp (fl a))])
  (unless (<= 0 a 1) (error 'find-repeat "bad input ~v" a))
  (define n (numerator a))
  (define d (denominator a))
  (define bfs (factorize b))
  (define de1
    (for/fold ([de1 0]) ([be (in-list bfs)])
      (match-define (list b e) be)
      (max de1
           (exact-ceiling (/ (max-dividing-power b d) e)))))
  (define d1 (expt b de1))
  (define STOP
    (max (add1 (exponent 999 #:base b))
         (- (exponent (/ 2.0 ulp) #:base b) de1)))
  (define d2- (/ d (gcd d d1)))
  (define de2
    (for/or ([de2 (in-naturals 1)]
             #:when (or (<= STOP de2)
                        (divides? d2- (sub1 (expt b de2)))))
      de2))
  (define d2 (sub1 (expt b de2)))
  (define d12 (* d1 d2))
  (define n12 (* d12 a))
  ; n12/d12 = n/d -> n12 = d12*a
  (define n1 (modulo (quotient (exact-round n12) d2) d1))
  (define n2 (min (- n12 (* d2 n1)) d2))
  (unless (= (+ (/ n1 d1) (/ n2 d12)) a) (error 'find-repeat "result mismatch"))
  (list (- (+ de1 de2)) n2 (- de1) n1))

;; ---------------------------------------------------------

(define (default-point base digits)
  (cond
    [(not (regexp-match? #rx"[.]" digits 0 base)) "."]
    [(not (regexp-match? #rx"[,]" digits 0 base)) ","]
    [(not (regexp-match? #rx"[•]" digits 0 base)) "•"]
    [else #false]))

(define (default-repeat base digits)
  (cond
    [(not (regexp-match? #rx"[_]" digits 0 base)) '("_" "_")]
    [(not (regexp-match? #rx"[‾]" digits 0 base)) '("‾" "‾")]
    [else #false]))

(define (default-etc base digits)
  (cond
    [(not (regexp-match? #rx"[.]" digits 0 base)) "..."]
    [(not (regexp-match? #rx"[…]" digits 0 base)) "…"]
    [else #false]))

(define (default-exponent base digits)
  (cond
    [(and (= 10 base) (not (regexp-match? #rx"e" digits 0 base)))
     "e"]
    [(and (< 1 base) (not (regexp-match? #rx"[*^]" digits 0 base)))
     (string #\* (string-ref digits 1) (string-ref digits 0) #\^)]
    [else #false]))

(define (default-exponent-sign sign)
  (match sign [(or #f '+) '++] [_ sign]))

(define (string-digits-conflict? s base digits)
  (define N (string-length s))
  (and
   (not (zero? N))
   (regexp-match?
    (string-append
     (regexp-quote (string (string-ref s 0)))
     "|"
     (regexp-quote (string (string-ref s (sub1 N)))))
    digits
    0
    base)))

;; number-repr->string : NumberRepr -> String
(module+ test
  (check-equal? (number-repr->string (number-repr 1 #() #() #(1 4) 6 0 #() 6))
                "0._14_")
  (check-equal? (number-repr->string (number-repr 1 #() #(1) #(4) 6 0 #() 6))
                "0.1_4_"))

(define (number-repr->string
         nr
         #:digits [digits digits-extended]
         #:sign [sign #f]
         #:point [point #f]
         #:repeat [repeat #f]
         #:etc [etc #f]
         #:exponent [exponent #f]
         #:exponent-sign [exponent-sign #f])
  (define base (number-repr-base nr))
  (define point* (or point (default-point base digits)))
  (define repeat* (or repeat (default-repeat base digits)))
  (define etc* (or etc (default-etc base digits)))
  (define exponent* (or exponent (default-exponent base digits)))
  (define exponent-sign* (or exponent-sign (default-exponent-sign sign)))
  (define (digit->char d) (string-ref digits d))
  (define (big->0string big)
    (define B (vector-length big))
    (cond
      [(zero? B) (string (digit->char 0))]
      [else (build-string B (λ (i) (digit->char (vector-ref big (- B i 1)))))]))
  (define (little->string little)
    (define L (vector-length little))
    (build-string L (λ (i) (digit->char (vector-ref little i)))))
  (define (repeat->string rep)
    (match-define (list begin-repeat end-repeat)
      (if (list? repeat*) repeat* (list repeat* "")))
    (cond
      [(not rep) etc*]
      [(zero? (vector-length rep)) ""]
      [else
       (when (string-digits-conflict? begin-repeat base digits)
         (error 'number->string "conflicting repeat and digits ~v" begin-repeat))
       (when (string-digits-conflict? end-repeat base digits)
         (error 'number->string "conflicting repeat and digits ~v" end-repeat))
       (string-append begin-repeat (little->string rep) end-repeat)]))
  (define (little-repeat->.string little repeat)
    (cond
      [(and (zero? (vector-length little))
            (or (not repeat) (zero? (vector-length repeat))))
       ""]
      [else
       (unless point*
         (error 'number->string "conflicting point and digits \".\""))
       (define point-N (string-length point*))
       (when (string-digits-conflict? point* base digits)
         (error 'number->string "conflicting sign and digits ~v" point*))
       (string-append point* (little->string little) (repeat->string repeat))]))
  (define (exponent->string exponent-sgn exponent-big)
    (match-define (list begin-exponent end-exponent)
      (if (list? exponent*) exponent* (list exponent* "")))
    (cond
      [(zero? exponent-sgn) ""]
      [else
       (string-append begin-exponent
                      (sign->begin-string exponent-sgn #:sign exponent-sign*)
                      (big->0string exponent-big)
                      (sign->end-string exponent-sgn #:sign exponent-sign*)
                      end-exponent)]))
  (match nr
    [(number-repr sgn big little repeat base expsgn expbig _)
     (define begin-sign (sign->begin-string sgn #:sign sign))
     (when (string-digits-conflict? begin-sign base digits)
       (error 'number->string "conflicting sign and digits ~v" begin-sign))
     (string-append begin-sign
                    (big->0string big)
                    (little-repeat->.string little repeat)
                    (exponent->string expsgn expbig)
                    (sign->end-string sgn #:sign sign))]))

(define (sign->begin-string s #:sign [sign #f])
  (match sign
    [#f (if (negative? s) "-" "")]
    ['+ (cond [(positive? s) "+"] [(negative? s) "-"] [else ""])]
    ['++ (if (negative? s) "-" "+")]
    ['parens (if (negative? s) "(" "")]
    [(list pos-ind zero-ind neg-ind)
     (define ind
       (cond [(positive? s) pos-ind] [(negative? s) neg-ind] [else zero-ind]))
     (match ind
       [(list b _) b]
       [(? string? b) b])]))

(define (sign->end-string s #:sign [sign #f])
  (match sign
    [(or #f '+ '++) ""]
    ['parens (if (negative? s) ")" "")]
    [(list pos-ind zero-ind neg-ind)
     (define ind
       (cond [(positive? s) pos-ind] [(negative? s) neg-ind] [else zero-ind]))
     (match ind
       [(list _ e) e]
       [_ ""])]))

;; string-regexp->rxs : (U String Regexp) -> String
(define (string-regexp->rxs sr)
  (cond
    [(string? sr) (regexp-quote sr)]
    [(regexp? sr) (object-name sr)]
    [else (error 'string-regexp->rxs "bad ~v" sr)]))

;; string-char-set->rxs : String -> String
(define (string-char-set->rxs s)
  (cond
    [(not (regexp-match? #rx"[]\\\\:^[-]" s))
     (string-append "[" s "]")]
    [else
     (string-append*
      (regexp-quote (string (string-ref s 0)))
      (for/list ([c (in-string s 1)])
        (string-append "|" (regexp-quote (string c)))))]))

;; string->number-repr : String -> NumberRepr
(define (string->number-repr s
                             #:base [base 10]
                             #:digits [digits digits-extended]
                             #:sign [sign #f]
                             #:point [point #f]
                             #:repeat [repeat #f]
                             #:etc [etc #f]
                             #:exponent [exponent #f]
                             #:exponent-sign [exponent-sign #f]
                             #:exponent-base [exponent-base base])
  (define point* (or point "."))
  (define repeat* (or repeat '("_" "_")))
  (define etc* (or etc "..."))
  (define exponent* (or exponent (default-exponent base digits)))
  (define exponent-sign* (or exponent-sign (default-exponent-sign sign)))
  (match-define (list begin-sign-rxs end-sign-rxs)
    (sign-rxs #:sign sign #:base base #:digits digits))
  (define digit-rxs (string-char-set->rxs (substring digits 0 base)))
  (define point-rxs (string-regexp->rxs point*))
  (match-define (list begin-repeat end-repeat) repeat*)
  (define begin-repeat-rxs (string-regexp->rxs begin-repeat))
  (define end-repeat-rxs (string-regexp->rxs end-repeat))
  (define etc-rxs (string-regexp->rxs etc*))
  (match-define (list begin-exponent-rxs end-exponent-rxs)
    (cond
      [exponent* (ind-rxs exponent*)]
      [else (list #f #f)]))
  (define exponent-digit-rxs
    (and exponent* (string-char-set->rxs (substring digits 0 exponent-base))))
  (match-define (list begin-exponent-sign-rxs end-exponent-sign-rxs)
    (cond
      [exponent*
       (sign-rxs #:sign exponent-sign* #:base exponent-base #:digits digits)]
      [else (list #f #f)]))
  (match s
    [(regexp
      (string-append
       "^(" begin-sign-rxs ")"
       "((" digit-rxs  ")*)"
       "((" point-rxs ")"
       "" "((" digit-rxs ")*)"
       "" "((" begin-repeat-rxs ")"
       "" "" "((" digit-rxs ")*)"
       "" "" "(" end-repeat-rxs ")"
       "" "" "|(" etc-rxs "))?)?"
       (cond
         [exponent*
          (string-append
           "((" begin-exponent-rxs ")"
           "" "(" begin-exponent-sign-rxs ")"
           "" "((" exponent-digit-rxs ")*)"
           "" "(" end-exponent-sign-rxs ")"
           "" "(" end-exponent-rxs "))?")]
         [else
          "(()()(())()())"])
       "(" end-sign-rxs ")$")
      (list _
            sgn1
            big1 _
            _ point1
            little1 _
            _ rep?1
            repeat1 _
            rep?2
            etc?1
            _ exp?1
            expsgn1
            expbig1 _
            expsgn2
            exp?2
            sgn2))
     (define sgn (strings->sign sgn1 sgn2 #:sign sign))
     (define big (string->big big1 #:digits digits))
     (define lit
       (cond
         [point1 (string->little little1 #:digits digits)]
         [else #()]))
     (define rep
       (cond
         [(and rep?1 rep?2) (string->little repeat1 #:digits digits)]
         [etc?1 #false]
         [else #()]))
     (define expsgn
       (cond
         [(and exponent* exp?1 exp?2)
          (strings->sign expsgn1 expsgn2 #:sign exponent-sign*)]
         [else 0]))
     (define expbig
       (cond
         [(and exponent* exp?1 exp?2)
          (string->big expbig1 #:digits digits)]
         [else #()]))
     (number-repr
      (cond
        [(and (for/and ([d (in-vector big)]) (zero? d))
              (for/and ([d (in-vector lit)]) (zero? d))
              (or (not rep) (for/and ([d (in-vector rep)]) (zero? d))))
         0]
        [else sgn])
      big
      lit
      rep
      base
      (cond
        [(for/and ([d (in-vector expbig)]) (zero? d)) 0]
        [else expsgn])
      expbig
      exponent-base)]
    [else #false]))

(define (ind-rxs ind)
  (match ind
    [(list b e) (list (string-regexp->rxs b) (string-regexp->rxs e))]
    [b (list (string-regexp->rxs b) "")]))

(define (sign-rxs #:sign sign #:base base #:digits digits)
  (match sign
    [(or #false '+ '++)
     (cond
       [(regexp-match? #rx"-" digits 0 base) (list "" "")]
       [(regexp-match? #rx"\\+" digits 0 base) (list "[-]?" "")]
       [else (list "[+-]?" "")])]
    ['parens
     (when (regexp-match? #rx"\\(" digits 0 base)
       (error 'string->number "conflicting sign and digits \"(\""))
     (list "\\(?" "\\)?")]
    [(list ind1 ind2 ind3)
     (match-define (list b1 e1) (ind-rxs ind1))
     (match-define (list b2 e2) (ind-rxs ind2))
     (match-define (list b3 e3) (ind-rxs ind3))
     (define (check-b b)
       (match (regexp-match b digits 0 base)
         [(or #false (cons "" _)) (void)]
         [(cons s _) (error 'string->number "conflicting sign and digits ~v" s)]))
     (check-b b1)
     (check-b b2)
     (check-b b3)
     (define b (string-append b1 "|" b2 "|" b3))
     (list (string-append b1 "|" b2 "|" b3)
           (string-append e1 "|" e2 "|" e3))]))

(define (strings->sign sgn1 sgn2 #:sign [sign #f])
  (match sign
    [(or #false '+ '++)
     (match sgn1
       [(or "" "+") 1]
       ["-" -1])]
    ['parens
     (match* [sgn1 sgn2]
       [["" ""] 1]
       [["(" ")"] -1])]
    [(list pos-ind zero-ind neg-ind)
     (match-define (list pos1 pos2) (ind-rxs pos-ind))
     (match-define (list zero1 zero2) (ind-rxs zero-ind))
     (match-define (list neg1 neg2) (ind-rxs neg-ind))
     (define p?
       (and (regexp-match-exact? pos1 sgn1) (regexp-match-exact? pos2 sgn2)))
     (define z?
       (and (regexp-match-exact? zero1 sgn1) (regexp-match-exact? zero2 sgn2)))
     (define n?
       (and (regexp-match-exact? neg1 sgn1) (regexp-match-exact? neg2 sgn2)))
     (match* [p? z? n?]
       [[#true _ #false] 1]
       [[#false _ #true] -1]
       [[#false #true #false] 0])]))

(define (char->digit c #:digits [digits digits-extended])
  (string-index digits c))

(define (string->big big1
                     #:digits [digits digits-extended])
  (define B (string-length big1))
  (build-vector B
    (λ (i) (char->digit (string-ref big1 (- B i 1)) #:digits digits))))

(define (string->little little1 #:digits [digits digits-extended])
  (define L (string-length little1))
  (build-vector L (λ (i) (char->digit (string-ref little1 i) #:digits digits))))
