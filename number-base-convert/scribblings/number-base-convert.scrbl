#lang scribble/manual
@(require racket/require
          scribble/example
          (for-label
           (subtract-in racket/base number-base-convert)
           racket/contract/base
           racket/format
           number-base-convert))

@title{number-base-convert}

@author{Alex Knauth}

@defmodule[number-base-convert]{
Converting between numbers and string representations of numbers in different bases.
}

@(define ev
   (make-base-eval
    '(require number-base-convert)))

@section{Number string conversion}

@defmodule[number-base-convert/number-string]{
Contains functions with many keyword arguments which can be
used to specify a base format.
}

@defproc[(number->string [n rational?]
                         [#:base base natural-number/c 10]
                         [#:sign sign sign-out/c #f]
                         [#:digits digits string? digits-extended]
                         [#:point point (or/c #f string?) #f]
                         [#:repeat repeat (or/c #f around-out/c) #f]
                         [#:etc etc (or/c #f string?) #f]
                         [#:notation notation
                          (or/c 'positional 'exponential
                                (-> rational? (or/c 'positional 'exponential)))
                          'positional]
                         [#:exponent exponent (or/c #f around-out/c) #f]
                         [#:exponent-sign exponent-sign sign-out/c #f]
                         [#:exponent-base exponent-base natural-number/c base])
         string?]{
Converts a number into a string according to the
configuration specified in the keyword arguments.

@itemize[

@item{
  @racket[base] --- the base used for the significand,
  and the number raised to the exponent in exponential
  notation.

  The default is decimal, base @racket[10].

  @examples[
    #:eval ev
    (eval:check (number->string 11 #:base 2) "1011")
    (eval:check (number->string 11 #:base 4) "23")
    (eval:check (number->string 11 #:base 6) "15")
    (eval:check (number->string 11 #:base 10) "11")
    (eval:check (number->string 11 #:base 12) "b")
    (eval:check (number->string 46 #:base 12) "3a")
    (eval:check (number->string 46 #:base 16) "2e")
  ]

  With the default @racket[digits], only bases up to 36 are
  supported, using numeric digits @litchar{0} through
  @litchar{9} and lowercase alphabetic digits @litchar{a}
  through @litchar{z}.

  For using uppercase alphabetic digits
  @litchar{A} through @litchar{Z} instead, use
  @racket[#:digits digits-extended-up].

  @examples[
    #:eval ev
    (eval:check (number->string 46 #:base 12 #:digits digits-extended-up) "3A")
    (eval:check (number->string 46 #:base 16 #:digits digits-extended-up) "2E")
  ]

  With @racket[#:digits digits-extended-up], bases up to 62
  are supported as well, using lowercase alphabetic digits
  after uppercase ones.

  @examples[
    #:eval ev
    (eval:check (number->string 1 #:base 48 #:digits digits-extended-up) "1")
    (eval:check (number->string 11 #:base 48 #:digits digits-extended-up) "B")
    (eval:check (number->string 46 #:base 48 #:digits digits-extended-up) "k")
    (eval:check (number->string 2878 #:base 48 #:digits digits-extended-up) "1Bk")
  ]}
]}

@defproc[(string->number [s string?])
         number?]{
Converts a string representation into a number.
}

@defthing[digits-extended string?]{
Contains numeric and lowercase-alphabetic digits, enough to
support bases up to 36.

@examples[
  #:eval ev
  (eval:check (string-length digits-extended) 36)
]}

@defthing[digits-extended-up string?]{
Contains numeric and alphabetic digits, including uppercase
and lowercase, enough to support bases up to 62.

@examples[
  #:eval ev
  (eval:check (string-length digits-extended-up) 62)
]}

@section{Specific base formats}

@subsection{Eridian seximal, base 6}

@defmodule[number-base-convert/eridian]{
Converts between numbers and strings in the Eridian seximal
or base 6 representation, using the digits @litchar{ℓ},
@litchar{I}, @litchar{V}, @litchar{λ}, @litchar{+}, and
@litchar{∀}.
}

@defproc[(number->eridian [n number?]) string?]{
Converts a number into a string containing an Eridian
seximal representation using the digits @litchar{ℓIVλ+∀}.

@examples[
  #:eval ev
  (eval:check (number->eridian 3043) "VVℓλI")
  (eval:check (number->eridian 1347) "IℓIVλ")
  (eval:check (number->eridian 12741) "Iλ+∀∀λ")
]}

@defproc[(eridian->number [s string?]) number?]{
Converts a string containing an Eridian seximal
representation with digits @litchar{ℓIVλ+∀} back into a
number.

@examples[
  #:eval ev
  (eval:check (eridian->number "VVℓλI") 3043)
  (eval:check (eridian->number "IℓIVλ") 1347)
  (eval:check (eridian->number "Iλ+∀∀λ") 12741)
]}

@section{Contracts}

@defthing[around-out/c flat-contract?]{

A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{flat contract}
equivalent to @racket[(or/c string? (list/c string? string?))]
that allows either a string or a list of two strings.

This matches the @racket[ind] local variable in the contract
for the @racket[sign] argument to the @racket[~r] function
from @racketmodname[racket/format].

If a value is just a string, that string goes before and
nothing goes after.
If a value is a list of two strings, the first string goes
before and the second string goes after.
Together they can go around something else.
}

@defthing[sign-out/c flat-contract?]{

A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{flat contract}
equivalent to
@racket[(or/c #f '+ '++ 'parens (list/c around-out/c around-out/c around-out/c))]
intended to allow values that express how signs should be
formatted.

This matches the contract for the @racket[sign] argument to
the @racket[~r] function from @racketmodname[racket/format].
}
