#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.1.9")
(define license 'MIT)   
(define collection "web-racket")
(define pkg-desc "web-racket - A Web Based GUI library, based on webui-wire")

(define scribblings
  '(
    ("scribblings/web-racket.scrbl" () (gui-library) "web-racket")
    ("scribblings/web-racket-version.scrbl" () (gui-library) "web-racket-version")
    ("scribblings/web-wire.scrbl" () (gui-library) "web-wire")
    ("scribblings/webui-wire-download.scrbl" () (gui-library) "webui-wire-download")
    ("scribblings/webui-wire-ipc.scrbl" () (gui-library) "webui-wire-ipc")
    )
  )

(define deps
  '("racket/base" "http-easy" "gregor" "html-printer" "simple-ini" "gregor-utils"
    "racket/gui" "net/sendurl"
    ))

(define build-deps
  '("racket-doc"
    "draw-doc"
    "rackunit-lib"
    "scribble-lib"
    ))
