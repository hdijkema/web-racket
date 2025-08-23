#lang racket/gui

(require "web-racket.rkt"
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Testing stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define test-menu
    '(("File" (("Open" open) ("Close" close) ("Quit" quit)))
      ("Edit" (("Copy" copy) ("Advanced" (("Copy 1" copy1) ("Copy 2" copy2)))
                              ("Cut" cut) ("Paste" paste)))
      ))

  (define test-window%
    (class ww-window%
      (super-new [html-file "../../web-wire/test/test1.html"])

      (begin
        (set-menu test-menu)
        )
      ))

); end of module
