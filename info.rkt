#lang info

;; Package info

(define collection 'multi)

(define deps
  '("base"
    "math-lib"
    "srfi-lite-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))

(define pkg-desc
  "Converting between numbers and string representations in different bases")

(define version "0.0")

(define license 'MIT)
