#lang racket/base

(provide number->eridian
         eridian->number
         digits-eridian)

(require "number-string.rkt")
(module+ test
  (require rackunit
           racket/math))

(define digits-eridian "ℓIVλ+∀")

;; ---------------------------------------------------------

;; number->eridian : Number -> String
;; eridian->number : String -> Number
(define (number->eridian n)
  (number->string n
                  #:base 6
                  #:digits digits-eridian
                  #:sign '("" "" "-")))

(define (eridian->number s)
  (string->number s
                  #:base 6
                  #:digits digits-eridian
                  #:sign '("" "" "-")))

(module+ test
  (define-check (check-number-eridian n s)
    (check-equal? (number->eridian n) s)
    (check-equal? (eridian->number s) n))
  (test-case "whole"
    (check-number-eridian 0 "ℓ")
    (check-number-eridian 1 "I")
    (check-number-eridian 2 "V")
    (check-number-eridian 3 "λ")
    (check-number-eridian 4 "+")
    (check-number-eridian 5 "∀")
    (check-number-eridian 6 "Iℓ")
    (check-number-eridian 8 "IV")
    (check-number-eridian 51 "IVλ")
    (check-number-eridian 78 "VIℓ")
    (check-number-eridian 310 "IVλ+")
    (check-number-eridian 726 "λVIℓ")
    (check-number-eridian 1347 "IℓIVλ")
    (check-number-eridian 1865 "IVλ+∀")
    (check-number-eridian 3043 "VVℓλI")
    (check-number-eridian 5910 "+λVIℓ")
    (check-number-eridian 11190 "IVλ+∀ℓ")
    (check-number-eridian 12741 "Iλ+∀∀λ")
    (check-number-eridian 44790 "∀+λVIℓ")
    (check-number-eridian -1 "-I")
    (check-number-eridian -2 "-V")
    (check-number-eridian -3 "-λ")
    (check-number-eridian -4 "-+")
    (check-number-eridian -5 "-∀")
    (check-number-eridian -6 "-Iℓ")
    (check-number-eridian -8 "-IV")
    (check-number-eridian -51 "-IVλ")
    (check-number-eridian -78 "-VIℓ")
    (check-number-eridian -310 "-IVλ+")
    (check-number-eridian -726 "-λVIℓ")
    (check-number-eridian -1865 "-IVλ+∀")
    (check-number-eridian -5910 "-+λVIℓ")
    (check-number-eridian -11190 "-IVλ+∀ℓ")
    (check-number-eridian -44790 "-∀+λVIℓ"))
  (test-case "small terminating"
    (check-number-eridian 1/2 "ℓ.λ")
    (check-number-eridian 1/3 "ℓ.V")
    (check-number-eridian 2/3 "ℓ.+")
    (check-number-eridian 1/4 "ℓ.Iλ")
    (check-number-eridian 3/4 "ℓ.+λ")
    (check-number-eridian 1/6 "ℓ.I")
    (check-number-eridian 5/6 "ℓ.∀"))
  (test-case "fifths"
    (check-number-eridian 1/5 "ℓ._I_") ; ℓ.(I)* repeating
    (check-number-eridian 2/5 "ℓ._V_")
    (check-number-eridian 3/5 "ℓ._λ_")
    (check-number-eridian 4/5 "ℓ._+_"))
  (test-case "sevenths"
    (check-number-eridian 1/7 "ℓ._ℓ∀_") ; ℓ.(ℓ∀)* repeating
    (check-number-eridian 2/7 "ℓ._I+_")
    (check-number-eridian 3/7 "ℓ._Vλ_")
    (check-number-eridian 4/7 "ℓ._λV_")
    (check-number-eridian 5/7 "ℓ._+I_")
    (check-number-eridian 6/7 "ℓ._∀ℓ_"))
  (test-case "eighths"
    (check-number-eridian 1/8 "ℓ.ℓ+λ")
    (check-number-eridian 3/8 "ℓ.VIλ")
    (check-number-eridian 5/8 "ℓ.λ+λ")
    (check-number-eridian 7/8 "ℓ.∀Iλ"))
  (test-case "ninths"
    (check-number-eridian 1/9 "ℓ.ℓ+")
    (check-number-eridian 2/9 "ℓ.IV")
    (check-number-eridian 4/9 "ℓ.V+")
    (check-number-eridian 5/9 "ℓ.λV")
    (check-number-eridian 7/9 "ℓ.++")
    (check-number-eridian 8/9 "ℓ.∀V"))
  (test-case "tenths"
    (check-number-eridian 1/10 "ℓ.ℓ_λ_") ; ℓ.ℓ(λ)* repeating
    (check-number-eridian 3/10 "ℓ.I_+_")
    (check-number-eridian 7/10 "ℓ.+_I_")
    (check-number-eridian 9/10 "ℓ.∀_V_"))
  (test-case "elevenths"
    (check-number-eridian  1/11 "ℓ._ℓλIλ+∀V+VI_") ; ℓ.(ℓλIλ+∀V+VI)* repeating
    (check-number-eridian  2/11 "ℓ._IℓλIλ+∀V+V_")
    (check-number-eridian  3/11 "ℓ._Iλ+∀V+VIℓλ_")
    (check-number-eridian  4/11 "ℓ._VIℓλIλ+∀V+_")
    (check-number-eridian  5/11 "ℓ._V+VIℓλIλ+∀_")
    (check-number-eridian  6/11 "ℓ._λIλ+∀V+VIℓ_")
    (check-number-eridian  7/11 "ℓ._λ+∀V+VIℓλI_")
    (check-number-eridian  8/11 "ℓ._+VIℓλIλ+∀V_")
    (check-number-eridian  9/11 "ℓ._+∀V+VIℓλIλ_")
    (check-number-eridian 10/11 "ℓ._∀V+VIℓλIλ+_"))
  (test-case "twelths"
    (check-number-eridian  1/12 "ℓ.ℓλ")
    (check-number-eridian  5/12 "ℓ.Vλ")
    (check-number-eridian  7/12 "ℓ.λλ")
    (check-number-eridian 11/12 "ℓ.∀λ"))
  (test-case "fourteenths"
    (check-number-eridian  1/14 "ℓ.ℓ_Vλ_")
    (check-number-eridian  3/14 "ℓ.I_I+_")
    (check-number-eridian  5/14 "ℓ.V_ℓ∀_")
    (check-number-eridian  9/14 "ℓ.λ_∀ℓ_")
    (check-number-eridian 11/14 "ℓ.+_+I_")
    (check-number-eridian 13/14 "ℓ.∀_λV_"))
  (test-case "fifteenths"
    (check-number-eridian  1/15 "ℓ.ℓ_V_")
    (check-number-eridian  2/15 "ℓ.ℓ_+_")
    (check-number-eridian  4/15 "ℓ.I_λ_")
    (check-number-eridian  7/15 "ℓ.V_+_")
    (check-number-eridian  8/15 "ℓ.λ_I_")
    (check-number-eridian 11/15 "ℓ.+_V_")
    (check-number-eridian 13/15 "ℓ.∀_I_")
    (check-number-eridian 14/15 "ℓ.∀_λ_"))
  (test-case "sixteenths"
    (check-number-eridian  1/16 "ℓ.ℓVIλ")
    (check-number-eridian  3/16 "ℓ.Iℓ+λ")
    (check-number-eridian  5/16 "ℓ.I∀Iλ")
    (check-number-eridian  7/16 "ℓ.Vλ+λ")
    (check-number-eridian  9/16 "ℓ.λVIλ")
    (check-number-eridian 11/16 "ℓ.+ℓ+λ")
    (check-number-eridian 13/16 "ℓ.+∀Iλ")
    (check-number-eridian 15/16 "ℓ.∀λ+λ"))
  (test-case "18ths"
    (check-number-eridian  1/18 "ℓ.ℓV")
    (check-number-eridian  5/18 "ℓ.I+")
    (check-number-eridian  7/18 "ℓ.VV")
    (check-number-eridian 11/18 "ℓ.λ+")
    (check-number-eridian 13/18 "ℓ.+V")
    (check-number-eridian 17/18 "ℓ.∀+"))
  (test-case "20ths"
    (check-number-eridian  1/20 "ℓ.ℓI_+_")
    (check-number-eridian  3/20 "ℓ.ℓ∀_V_")
    (check-number-eridian  7/20 "ℓ.Vℓ_λ_")
    (check-number-eridian  9/20 "ℓ.V+_I_")
    (check-number-eridian 11/20 "ℓ.λI_+_")
    (check-number-eridian 13/20 "ℓ.λ∀_V_")
    (check-number-eridian 17/20 "ℓ.∀ℓ_λ_")
    (check-number-eridian 19/20 "ℓ.∀+_I_"))
  (test-case "PHM"
    (define (floor-nearest n m)
      (define q (exact-floor (/ n m)))
      (* q m))
    (define (wrap n m)
      (- n (floor-nearest n m)))
    (define (avg a . bs)
      (/ (apply + a bs) (add1 (length bs))))
    (define (geomean a . bs)
      (expt (apply * a bs) (/ (add1 (length bs)))))
    (check-equal? (wrap 9 4.5) 0.0)
    (check-equal? (wrap 10.5 4.5) 1.5)
    (check-equal? (wrap -3 4.5) 1.5)
    (check-equal? (wrap -3.5 4.5) 1.0)
    (check-equal? (wrap -4.5 4.5) 0.0)

    ;; -----------------------------------------------------
 
    (define Eridian-sleep-min-eridian-display
      (number->eridian (* 4 (expt 6 4))))
    (define Eridian-sleep-max-eridian-display
      (number->eridian (* 14 (expt 6 4))))
    (define Eridian-sleep-avg-eridian-display
      (number->eridian (avg (* 4 (expt 6 4)) (* 14 (expt 6 4)))))
    (define Eridian-sleep-geomean-eridian-display
      (number->eridian (exact-round (geomean (* 4 (expt 6 4)) (* 14 (expt 6 4))))))
    (check-equal? Eridian-sleep-min-eridian-display "+ℓℓℓℓ")
    (check-equal? Eridian-sleep-max-eridian-display "VVℓℓℓℓ")
    (check-equal? Eridian-sleep-avg-eridian-display "Iλℓℓℓℓ")
    (check-equal? Eridian-sleep-geomean-eridian-display "IIV∀VV")

    ;; -----------------------------------------------------

    (define C11-near-end-eridian-display "IℓIVλ")
    (define C11-near-end-eridian-seconds
      (eridian->number C11-near-end-eridian-display))
    (check-equal? C11-near-end-eridian-seconds 1347)
    (check-equal? (number->eridian
                   (wrap (+ (eridian->number "+II+I")
                            (eridian->number "VVℓλI"))
                         (expt 6 5)))
                  "λVIV")

    ;; -----------------------------------------------------

    (check-equal? (number->eridian (exact-round C11-near-end-eridian-seconds))
                  "IℓIVλ")
    
    (check-equal? (number->eridian
                   (+ (eridian->number "IℓIVλ")
                      (eridian->number "IλVVℓ+")))
                  "I+VλλI")
    
    (check-equal? (number->eridian
                   (- (eridian->number "Iλ+∀∀λ")
                      (eridian->number "IℓIVλ")))
                  "IV++λℓ")
    (check-equal? (number->eridian
                   (+ (eridian->number "IℓIVλ")
                      (eridian->number "Iλℓℓℓℓ")))
                  "I+ℓIVλ")
    (check-equal? (number->eridian
                   (+ (eridian->number "IℓIVλ")
                      (eridian->number "IIV∀VV")))
                  "IVλℓ+∀")
    (check-equal? (number->eridian
                   (- (eridian->number "λ+∀∀λ")
                      (eridian->number "Vλℓ+∀")))
                  "II∀ℓ+")
    (check-equal? (number->eridian
                   (+ (eridian->number "λ+∀∀λ")
                      (eridian->number "VVℓλI")))
                  "IℓIℓV+")
    (check-equal? (number->eridian
                   (exact-round
                    (avg (eridian->number "II∀ℓ+")
                         (eridian->number "IV++λℓ"))))
                  "∀ℓI+∀")
    (check-equal? (number->eridian
                   (exact-round
                    (avg (eridian->number "VVℓλI")
                         (eridian->number "∀ℓI+∀"))))
                  "λ+IIℓ")
    (check-equal? (number->eridian
                   (+ (eridian->number "λ+∀∀λ")
                      (eridian->number "λ+IIℓ")))
                  "IIλIℓλ")
    (check-equal? (number->eridian
                   (- (eridian->number "IIℓ+ℓV")
                      (eridian->number "λ+∀∀λ")))
                  "λI+ℓ∀")

    (check-equal? (number->eridian
                   (+ (eridian->number "IℓIVλ")
                      (eridian->number "VℓℓVλ∀")))
                  "VIℓ+ℓV")
    
    (check-equal? (number->eridian
                   (wrap (eridian->number "VIℓ+ℓV") (expt 6 5)))
                  "Iℓ+ℓV")
    
    (void))
  )
