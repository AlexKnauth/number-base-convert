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

@subsection{Hexanonary, HEN, base 54}

@defmodule[number-base-convert/hexanonary]

@defproc[(number->hexanonary [n number?]) string?]

@defproc[(hexanonary->number [s string?]) number?]

@subsection{Heptoctal, HET, base 56}

@defmodule[number-base-convert/heptoctal]

@defproc[(number->heptoctal [n number?]
                            [#:digits digits string? digits-heptoctal-py])
         string?]

@defproc[(heptoctal->number [s string?]
                            [#:digits digits string? digits-heptoctal-py])
         number?]

@defthing[digits-heptoctal-py string?]{
The default digit set for heptoctal number representation.
Uses alphanumeric characters excluding @litchar{0Oo} and
@litchar{1Il} to avoid potential visual confusion.
They are in sorted order with numeric characters first, then
uppercase alphebetic characters next, then lowercase
alphabetic characters last.

This matches jyn514's Python base56 library here:
@url["https://github.com/jyn514/base56"]

It also matches the @litchar{Py3} character set from the Go
base56 library here:
@url["https://pkg.go.dev/toolman.org/encoding/base56"]
}

@defthing[digits-heptoctal-php string?]{
Similar to @racket[digits-heptoctal-py], except that it
doesn't use that sorted order, but uses numeric lowercase
uppercase instead.

This matches a PHP base56 implementation here:
@url["https://rossduggan.ie/blog/codetry/base-56-integer-encoding-in-php/index.html"]

It also matches the @litchar{Alt} character set from the Go
base56 library here:
@url["https://pkg.go.dev/toolman.org/encoding/base56"]
}

@defthing[digits-heptoctal-go string?]{
Yet another digit set, also alphanumeric but excluding
different characters @litchar{ODQo} and @litchar{Ii}.
Note that this one does include @litchar{0} which has some
danger of being confused with @litchar{O}, and also includes
both @litchar{1} and @litchar{l}, which have some danger of
being confused with each other or with @litchar{I}.

This matches the @litchar{Std} character set from the Go
base56 library here:
@url["https://pkg.go.dev/toolman.org/encoding/base56"]
}

@defthing[digits-heptoctal-wikipedia string?]{
Yet another digit set, also alphanumeric but excluding
different characters @litchar{0Oo} and @litchar{Iil}.
Note that this one does include @litchar{1}, which has some
danger of being confused with @litchar{I} or @litchar{l}.

The Wikipedia article describing this just cites the same
PHP implementation that @racket[digits-heptoctal-php] is
based on, so it's possible that this was just created from a
misunderstanding:
@url["https://en.wikipedia.org/wiki/Binary-to-text_encoding#Encoding_standards"]
}

@subsection{Bintetraseptimal, BNT, base 58}

@defmodule[number-base-convert/bintetraseptimal]

@defproc[(number->bintetraseptimal
          [n number?]
          [#:digits digits string? digits-bintetraseptimal-btc])
         string?]

@defproc[(bintetraseptimal->number
          [s string?]
          [#:digits digits string? digits-bintetraseptimal-btc])
         number?]

@defthing[digits-bintetraseptimal-btc string?]{
The default digit set for bintetraseptimal number
representation.
Uses alphanumeric characters excluding @litchar{0O} and
@litchar{Il} to avoid some potential visual confusion.
They are in sorted order with numeric characters first, then
uppercase alphebetic characters next, then lowercase
alphabetic characters last.

Matches a base58 implementation for bitcoin addresses.
}

@defthing[digits-bintetraseptimal-flickr string?]{
Similar to @racket[digits-bintetraseptimal-btc], except that
it doesn't use that sorted order, but uses numeric lowercase
uppercase instead.

Matches a base58 implementation for Flickr Short URLs here:
@url["https://www.flickr.com/groups/api/discuss/72157616713786392/"]
}

@subsection{Binpentaseximal, BIP, base 62}

@defmodule[number-base-convert/binpentaseximal]

@defproc[(number->binpentaseximal [n number?]) string?]

@defproc[(binpentaseximal->number [s string?]) number?]

@subsection{Octoctal, OCC, base 64}

@defmodule[number-base-convert/octoctal]

@defproc[(number->octoctal [n number?] [#:digits digits string? digits-octoctal])
         string?]

@defproc[(octoctal->number [s string?] [#:digits digits string? digits-octoctal])
         number?]

@defthing[digits-octoctal string?]{
The default digit set for octoctal number representation.
Uses alphanumeric characters as well as @litchar{+} and
@litchar{/}.
Note that this is not url or filename safe, and it isn't in
sorted order either.

Matches the digits of base64 from RFC 4648 section 4.
}

@defthing[digits-octoctal-url string?]{
A url and filename safe digit set for octoctal numbers.
Uses alphanumeric characters as well as @litchar{-} and
@litchar{_}.

Matches the digits of base64url from RFC 4648 section 5.
}

@subsection{Pentasuboptimal, PES, base 85}

@defmodule[number-base-convert/pentasuboptimal]

@defproc[(number->pentasuboptimal [n number?]) string?]

@defproc[(pentasuboptimal->number [s string?]) number?]

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
