#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.1.0")
(define license 'GPL-3.0-or-later)   ; The liboa library has this license
(define collection "web-racket")
(define pkg-desc "web-racket - A Web Based GUI library, based on web-wire")

(define scribblings
  '(
    ("scribblings/web-racket.scrbl" () (gui-library) "web-racket")
    )
  )

(define deps
  '("racket/base" "net/http-easy" "file/unzip"))

(define build-deps
  '("racket-doc"
    "draw-doc"
    "rackunit-lib"
    "scribble-lib"
    ))
