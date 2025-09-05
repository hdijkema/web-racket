(module sprintf racket/base

  (require racket/format
           racket/string
           )

  (provide sprintf
           sprintf*
           )

  (define re-format
    #px"([^%]*)[%]([0-]{0,1})([1-9][0-9]*|[*]){0,1}([.]([0-9]+|[*])){0,1}(l*)([%dfsx])")

  (define-syntax shift
    (syntax-rules ()
      ((_ args)
       (let ((first (car args)))
         (set! args (cdr args))
         first))))

  (define (format-part zeros adjust-width precision kind arg)
    (if (number? arg)
        (let* ((pad-str (if (eq? zeros #f) " "
                            (if (string=? zeros "0")
                                "0"
                                " ")))
               (adjust   (if (eq? zeros #f) 'right
                             (if (string=? zeros "-") 'left 'right)))
               (min-width (if (eq? adjust-width #f) 1 adjust-width))
               (precision (if (eq? precision #f) 0
                              (if (eq? kind 'd)
                                  0
                                  precision)))
               (base (if (eq?  kind 'x) 16 10))
               )
          (when (eq? kind 's)
            (error "argument is a number, but string expected"))
          (let ((r (~r arg #:pad-string pad-str #:min-width min-width #:precision precision #:base base)))
            (if (eq? adjust 'left)
                (let ((r-trim (string-trim r)))
                  (string-append r-trim
                                 (make-string
                                  (- (string-length r) (string-length r-trim))
                                   #\space)))
                r)))
        (let* ((pad-str (if (string=? zeros "") " " zeros))
               (min-width (if (eq? adjust-width #f) 0 adjust-width))
               (max-width (if (eq? precision #f) +inf.0 precision))
               (adjust (if (eq? zeros #f) 'left
                           (if (string=? zeros "-") 'left 'right)))
               )
          (unless (eq? kind 's)
            (error "argument is a string, but a number is expected"))
          (~a arg #:pad-string pad-str #:min-width min-width #:max-width max-width #:align adjust))
        )
    )

  (define-syntax fmt
    (syntax-rules ()
      ((_ a ...)
       (format a ...))))
  
  (define (do-format format args)
    (if (null? args)
        (let ((m (regexp-match re-format format)))
          (unless (eq? m #f)
            (error (fmt "formatting left, but no arguments left: ~a" format)))
          format)
        (let ((m (regexp-match re-format format)))
          (when (eq? m #f)
            (error (fmt "arguments left, but no formatting left: ~a" format)))
          (let* ((matched-length (string-length (list-ref m 0)))
                 (prefix (list-ref m 1))
                 (zeros (list-ref m 2))
                 (adjust-width (list-ref m 3))
                 (precision (list-ref m 5))
                 (long (list-ref m 6))
                 (kind (string->symbol (list-ref m 7)))
                 )
            (unless (eq? adjust-width #f)
              (set! adjust-width (if (string=? adjust-width "*")
                                     (let ((n (shift args)))
                                       (when (null? args)
                                         (error "* requires >= 2 arguments left"))
                                       (unless (number? n)
                                         (error "* requires a number?"))
                                       n)
                                     (string->number adjust-width))))
            (unless (eq? precision #f)
              (set! precision (if (string=? precision "*")
                                  (let ((n (shift args)))
                                    (when (null? args)
                                      (error "* requires >= 2 arguments left"))
                                    (unless (number? n)
                                      (error "* requires a number?"))
                                    n)
                                  (string->number precision))))
            (string-append prefix
                           (if (eq? kind '%)
                               "%"
                               (format-part zeros adjust-width precision kind (shift args)))
                           (do-format (substring format matched-length) args))))
        )
    )
                                     
  
  (define (sprintf format . args)
    (do-format format args))

  (define (sprintf* format args)
    (do-format format args))
    
  
  ) ; end of module