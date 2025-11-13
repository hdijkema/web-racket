(module web-racket-ini racket/gui

  (require simple-ini/class
           "../private/web-racket.rkt"
           )

  (provide ww-simple-ini%)
  

  (define ww-simple-ini%
    (class ww-settings%
      (init-field [ini (error "Initialize this ww-simple-ini% with a ini% object of the simple-ini package")]
                  [section (error "Initialize this ww-simple-ini% with a section to use in the ini")]
                  )

      (super-new)

      (define/override (set key value)
        (send ini set! section key value)
        this
        )

      (define/override (get key . default)
        (let ((d (if (null? default) #f (car default))))
          (send ini get section key d)))

      (define/override (clone new-section)
        (new ww-simple-ini% [ini ini] [section new-section]))

      (begin
        (unless (is-a? ini ini%)
          (error "ini must be of type ini%"))
        (unless (symbol? section)
          (error "section must be a symbol"))
      ))
    )
  
  
  ) ; end of module
