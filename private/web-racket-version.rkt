(module web-racket-version racket/base

  (require racket/string)

  (provide ww-version
           ww-version-major
           ww-version-minor
           ww-version-patch
           ww-wire-version
           ww-wire-version-major
           ww-wire-version-minor
           ww-wire-version-patch
           )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Utils
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (mk-version a b c)
    (string-join 
            (map number->string (list a b c))
            "."))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web Wire Version
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ww-version-major 0)
  (define ww-version-minor 1)
  (define ww-version-patch 3)

  (define ww-version (mk-version ww-version-major ww-version-minor ww-version-patch))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web Wire IPC Version
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define ww-wire-version-major 0)
  (define ww-wire-version-minor 2)
  (define ww-wire-version-patch 8)

  (define ww-wire-version (mk-version ww-wire-version-major ww-wire-version-minor ww-wire-version-patch))

  )