(module webui-wire-ipc racket/base

  (require "webui-wire-download.rkt"
           racket/string
           )
  
  (provide webui-ipc)

  (define re-kind #px"^([^:]+)[:]")

  (define (is-int? str)
    (let ((re-num #px"^[0-9]+$"))
      (if (regexp-match re-num str)
          #t
          #f)))

  ;; This function only expects one character, namely \n
  ;; In windows disable text mode on stdout/stderr of webui-wire.
  (define (read-eol port)
    (read-string 1 port))
  
  (define (process-stderr-reader process-stderr event-queuer log-processor)
    (thread (λ ()
              (letrec ((reader (λ () 
                                 (let* ((str-length (read-string 8 process-stderr))
                                        (colon (read-string 1 process-stderr)))
                                   (if (eof-object? colon)
                                       (begin
                                         (log-processor 'stderr-reader "webui-wire executable exited")
                                         'process-ended)
                                       (begin
                                         (if (and (string? colon) (string=? colon ":") (is-int? str-length))
                                             ; process line
                                             (let* ((length (string->number str-length))
                                                    (input (read-string length process-stderr))
                                                    (m (regexp-match re-kind input))
                                                    )
                                               (read-eol process-stderr)
                                               (if (eq? m #f)
                                                   (log-processor 'stderr-reader
                                                                  (format "Unexpected: no kind: input = ~a" input))
                                                   (let ((kind (string->symbol (list-ref m 1)))
                                                         (line (substring input (string-length (car m))))
                                                         )
                                                     (if (eq? kind 'EVENT)
                                                         (event-queuer line)
                                                         (log-processor kind line))))
                                               )
                                             ; otherwise skip line
                                             (let* ((line (read-line process-stderr))
                                                    (msg (string-trim (string-append str-length colon line))))
                                               (log-processor 'stderr-reader msg))
                                             )
                                         (reader)
                                         )
                                       ))
                                 )
                               ))
                (reader)))
            )
    )

  (define (webui-ipc event-queuer log-processor)
    (let* ((webui-wire-exe (ww-webui-wire))
           (proc-args (append (list #f #f #f) webui-wire-exe))
           )
      (call-with-values
       (λ () (apply subprocess proc-args))
       (λ (pid process-stdout process-stdin process-stderr)
         (let ((reader-thrd (process-stderr-reader process-stderr event-queuer log-processor)))
           (λ (cmd)
             (displayln cmd process-stdin)
             (flush-output process-stdin)
             (let* ((str-length (read-string 8 process-stdout))
                    (colon (read-string 1 process-stdout)))
               ;(displayln (format "len: ~a, str-length: ~a, colon: ~a" (string-length str-length) str-length colon))
               (unless (and (string? colon)
                            (string=? colon ":"))
                 (error "Unexpected input from webui-wire executable"))
               (let* ((length (string->number str-length))
                      (input (read-string length process-stdout))
                      )
                 (read-eol process-stdout)
                 input)))))
       ))
    )
        

  )