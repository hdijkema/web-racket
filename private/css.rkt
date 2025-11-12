(module css racket/base

  (require racket/string)

  (provide css-style
           css-style->list

           set-css!
           get-css
           clear-css!
           css-keys

           css-style?

           string->css-style
           css-style->string

           stylesheet
           stylesheet?
           
           stylesheet-set!
           stylesheet-get
           stylesheet-clear!
           stylesheet-keys

           stylesheet->string
           string->stylesheet
           )

  (define-struct style
    (
     [style #:auto #:mutable]
     )
    #:auto-value #f
    #:transparent)

  (define-struct css-stylesheet
    (
     [sheet #:auto #:mutable]
     )
     #:auto-value (make-hashalw)
    #:transparent)

  (define st-style style-style)
  (define st-style! set-style-style!)
  (define make-st make-style)
  (define st? style?)
  (define stylesheet? css-stylesheet?)
  (define make-stylesheet make-css-stylesheet)
  (define stylesheet-sheet css-stylesheet-sheet)
  
  
  (define (css-style style_or_styles . args)
    (if (symbol? style_or_styles)
        (let ((css (if (null? args) "" (car args)))
              (st (make-st)))
          (st-style! st (make-hash))
          (hash-set! (st-style st) style_or_styles css)
          st)
        (let* ((st (make-st))
               (h (begin (st-style! st (make-hash)) (st-style st))))
          (for-each (lambda (st)
                      (let ((entry (car st))
                            (css (cadr st)))
                        (hash-set! h entry (format "~a" css))))
                    style_or_styles)
          st)))

  (define (set-css! st entry css)
    (hash-set! (st-style st) entry css)
    st)

  (define (get-css st entry)
    (hash-ref (st-style st) entry ""))

  (define (clear-css! st entry)
    (hash-remove! (st-style st) entry))

  (define (css-style? st)
    (st? st))

  (define (css-keys st)
    (hash-keys (st-style st)))

  (define (css-style->list st)
    (let* ((h (st-style st))
           (keys (hash-keys h)))
      (map (lambda (k)
             (list k (hash-ref h k)))
           keys)))

  (define (css-style->string st . custom-sep)
    (let* ((sep (if (null? custom-sep) " " (car custom-sep)))
           (h (st-style st))
           (keys (hash-keys h)))
      (letrec ((f (lambda (keys)
                    (if (null? keys)
                        ""
                        (let ((key (car keys)))
                          (string-append (symbol->string key)
                                         ": "
                                         (hash-ref h key)
                                         ";"
                                         sep
                                         (f (cdr keys))))
                        ))
                  ))
        (string-trim (f keys)))))


  (define re-style-split #px"\\s*[;]\\s*")
  (define re-style-kv-split #px"\\s*[:]\\s*")
  
  (define (split-style-string style)
    (let ((sp-style (regexp-split re-style-split style)))
      (letrec ((f (lambda (entries)
                    (if (null? entries)
                        '()
                        (let* ((entry (string-trim (car entries)))
                               (kv (regexp-split re-style-kv-split entry))
                               (key (car kv))
                               (skey (if (string? key)
                                         (string->symbol key)
                                         key))
                               (val (if (= (length kv) 2) (cadr kv) ""))
                               (keyval (list skey val))
                               )
                          (if (string=? entry "")
                              (f (cdr entries))
                              (cons keyval (f (cdr entries)))))
                        ))
                  ))
        (f sp-style))))


  
  (define (string->css-style style-str)
    (css-style (split-style-string style-str)))


  (define (stylesheet entry_or_entries . style)
    (if (symbol? entry_or_entries)
        (let* ((st (car style))
               (ss (make-stylesheet))
               (h (stylesheet-sheet ss)))
          (if (css-style? st)
              (begin
                (hash-set! h entry_or_entries st)
                ss)
              (error "A css-style is expected")))
        (let* ((ss (make-stylesheet))
               (h (stylesheet-sheet ss)))
          (for-each (lambda (entry)
                      (let* ((key (car entry))
                             (s (cadr entry)))
                        (if (css-style? s)
                            (hash-set! h key s)
                            (error (format "A css-style is expected for ~a"
                                           key)))
                        ))
                    entry_or_entries)
          ss)))

  (define (string->stylesheet str)
    (error "Not implemented yet")
    #t)

  (define (stylesheet-entry->string e)
    (if (list? e)
        (if (null? e)
            ""
            (string-append (stylesheet-entry->string (car e))
                           " "
                           (stylesheet-entry->string (cdr e))))
        (format "~a" e)))

  (define (stylesheet->string ss)
    (let* ((h (stylesheet-sheet ss))
           (keys (hash-keys h))
           (sep "\n"))
      (letrec ((f (lambda (keys)
                    (if (null? keys)
                        ""
                        (let* ((key (car keys))
                               (st (hash-ref h key)))
                          (string-append (stylesheet-entry->string key) " {\n  "
                                         (css-style->string  st "\n  ")
                                         "\n}\n"
                                         (f (cdr keys))))))
                  ))
        (f keys))))

  (define (stylesheet-set! ss key style)
    (let ((h (stylesheet-sheet ss)))
      (hash-set! h key style)
      ss))

  (define (stylesheet-get ss key)
    (let ((h (stylesheet-sheet ss)))
      (hash-ref h key (make-st))))

  (define (stylesheet-clear! ss key)
    (let ((h (stylesheet-sheet ss)))
      (hash-remove! h key)))

  (define (stylesheet-keys ss)
    (let ((h (stylesheet-sheet ss)))
      (hash-keys h)))
  
  
  ); end of module