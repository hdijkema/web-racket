(module web-racket-version racket/base

  (require racket/string)

  (provide ww-version
           ww-version-major
           ww-version-minor
           ww-version-patch
           ww-ffi-version
           ww-ffi-version-major
           ww-ffi-version-minor
           ww-ffi-version-patch
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
  ;; Web Wire FFI Version
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define ww-ffi-version-major 0)
  (define ww-ffi-version-minor 2)
  (define ww-ffi-version-patch 1)

  (define ww-ffi-version (mk-version ww-ffi-version-major ww-ffi-version-minor ww-ffi-version-patch))

  )