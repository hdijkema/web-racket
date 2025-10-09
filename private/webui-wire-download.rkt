(module webui-wire-download racket/base

  (require setup/dirs
           net/sendurl
           net/url
           file/unzip
           racket/file
           )

  
  (provide ww-current-win-release
           ww-download-if-needed
           )
           

  (define ww-current-win-release "https://github.com/hdijkema/web-wire/releases/download/0.1/web-wire-0.1-win64.zip")
  
  (define user-web-wire-location #f)

  (define (ww-set-web-wire-location! path-or-dir)
    (set! user-web-wire-location (build-path path-or-dir))
    user-web-wire-location)

  (define (os)
    (format "~a-~a" (system-type) (system-type 'word)))

  (define (web-wire-exe)
    (if (eq? (system-type) 'windows)
        "web-wire.exe"
        "web-wire"))

  (define (web-wire-dir)
    (if (eq? user-web-wire-location #f)
        (let* ((cache-dir (find-system-path 'cache-dir))
               (os-dir (build-path cache-dir (os)))
               (web-wire-prg (build-path os-dir (web-wire-exe)))
               )
          (unless (file-exists? web-wire-prg)
            (error "Web wire executable not found: '~a'" web-wire-prg))
          os-dir)
        (let ((web-wire-prg (build-path user-web-wire-location (web-wire-exe))))
          (unless (file-exists? web-wire-prg)
            (error "Web wire executable not found: '~a'" web-wire-prg))
          user-web-wire-location)
        ))

  (define (web-wire-prg)
    (build-path (web-wire-dir) (web-wire-exe)))

  (define (do-download-and-extract release version os-dir)
    (let* ((url (string->url release))
           (port-in (get-pure-port url #:redirections 10))
           (release-file (build-path os-dir "release.zip"))
           (port-out (open-output-file release-file #:exists 'replace))
           )
      (letrec ((f (lambda (count next-c len)
                    (let ((bytes (read-bytes 16384 port-in)))
                      (if (eof-object? bytes)
                          count
                          (let ((read-len (bytes-length bytes)))
                            (when (> read-len 0)
                              (set! count (+ count read-len))
                              (when (> count next-c)
                                (display (format "~a..." count))
                                (set! next-c (+ count len)))
                              (write-bytes bytes port-out)
                              )
                            (f count next-c len)))))
                  ))
        (display "Downloading web-wire...")
        (let ((count (f 0 0 10000000)))
          (displayln (format "~a downloaded" count)))
        (close-input-port port-in)
        (close-output-port port-out)
        (display "Unzipping...")
        (unzip release-file
               (make-filesystem-entry-reader #:dest os-dir
                                             #:strip-count 1
                                             #:exists 'replace)
               )
        (display "removing zip file...")
        (delete-file release-file)
        (displayln "done")
        )))

  (define (ww-download-if-needed release)
    (let* ((os-dir (web-wire-dir))
           (re #px"web[-]wire[-]([0-9.]+)[-]")
           )
      (unless (directory-exists? os-dir)
        (make-directory* os-dir))
      (let ((m (regexp-match re release)))
        (unless (eq? m #f)
          (let* ((version-file (build-path os-dir "version"))
                 (version (cadr m))
                 (has-version #f))
            (when (file-exists? version-file)
              (let ((file-version (file->value version-file)))
                (when (string=? file-version version)
                  (set! has-version #t))))
            (unless has-version
              (do-download-and-extract release version os-dir)
              (write-to-file version version-file)
              ))
          ))))  
  )