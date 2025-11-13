(module webui-wire-download racket/base

  (require setup/dirs
           net/sendurl
           net/url
           file/unzip
           racket/file
           "web-racket-version.rkt"
           racket/system
           )

  (provide ww-set-custom-webui-wire-command!
           ww-get-webui-wire-command
           )
  
  
  (define (ww-get-webui-wire-command)
    (unless (webui-wire-exists?)
      (error "webui-wire needs to be installed in order to use web-racket"))
    (get-webui-wire-cmd))

  (define user-webui-wire-command #f)

  (define (ww-set-custom-webui-wire-command! cmd)
    (set! user-webui-wire-command cmd)
    user-webui-wire-command)

  (define (get-webui-wire-cmd)
    (if (eq? user-webui-wire-command #f)
        (let ((os (system-type 'os*)))
          (if (eq? os 'linux)
              "flatpak run nl.dijkewijk.webui-wire"
              (format "~a"
                      (build-path (webui-wire-dir) (if (eq? os 'windows)
                                                     "webui-wire.exe"
                                                     "webui-wire")))))
        user-webui-wire-command))
          
  (define (webui-wire-dir)
    (let* ((cache-dir (find-system-path 'cache-dir))
           (ww-dir (build-path cache-dir "webui-wire"))
           )
      (unless (directory-exists? ww-dir)
        (make-directory ww-dir))
      ww-dir))

  (define (flatpak-ok? str)
    (let* ((re #px"([0-9]+)[.]([0-9]+)[.]([0-9]+)$")
           (m  (regexp-match re str))
           (v  (if (eq? m #f)
                   0
                   (let ((l (map string->number (cdr m))))
                     (+ (* (car l) 10000) (* (cadr l) 100) (caddr l))))))
      (>= 11000)))


  (define (webui-wire-exists?)
    (let ((os (system-type 'os*)))
      (cond [(eq? os 'linux)
             (webui-wire-exists-linux?)]
            [(eq? os 'windows)
             (webui-wire-exists-windows?)]
            [else
             (error
              (format
               "Currently not implemented operating system '~a'" os))]
            )
      )
    )


  (define (webui-wire-exists-linux?)
    (let ((flatpak (call-with-values (lambda () (process "flatpak --version"))
                                     (lambda (args)
                                       (let ((out (car args)))
                                         (read-line out))))))
      (unless (string? flatpak)
        (error "Please install flatpak to use web-racket"))
      (unless (flatpak-ok? flatpak)
        (error (format "Not the right flatpak version installed: ~a" flatpak)))
      (let ((webui-wire (call-with-values (lambda () (process "flatpak list --user | grep webui-wire"))
                                          (lambda (args)
                                            (let ((out (car args)))
                                              (read-line out))))))
        (if (string? webui-wire)
            (let ((webui-wire-version (call-with-values (lambda () (process "flatpak run nl.dijkewijk.webui-wire --version"))
                                                        (lambda (args)
                                                          (let ((out (car args))
                                                                (in (cadr args)))
                                                            (displayln "exit" in)
                                                            (flush-output in)
                                                            (read-line out))))))
              (if (string=? webui-wire-version ww-wire-version)
                  #t
                  (begin
                    (system "flatpak uninstall --user --noninteractive --assumeyes nl.dijkewijk.webui-wire")
                    (download-webui-wire-linux)))
              )
            (download-webui-wire-linux)))
      )
    )

  (define (webui-wire-exists-windows?)
    (let ((webui-wire-exe (get-webui-wire-cmd 'windows)))
      (if (file-exists? webui-wire-exe)
          #t
          (download-webui-wire-windows))
      )
    )


  (define (download-webui-wire-linux)
    (let* ((download-link (current-webui-wire-link))
           (filepath (do-download download-link "webui-wire.flatpak")))
      (system (format "flatpak install --user --assumeyes --noninteractive \"~a\"" filepath))
      #t
      )
    )

  (define (download-webui-wire-windows)
    (let* ((download-link (current-webui-wire-link))
           (filepath (do-download download-link "webui-wire.exe")))
      (displayln filepath)
      #t
      )
    )

  (define (do-download link filename)
    (let* ((url (string->url link))
           (port-in (get-pure-port url #:redirections 10))
           (filepath (build-path (webui-wire-dir) filename))
           (port-out (open-output-file filepath #:exists 'replace))
           )
      (letrec ((downloader-func (λ (count next-c len)
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
                                          (downloader-func count next-c len)))))
                                ))
        (display (format "Downloading webui-wire (~a)..." link))
        (let ((count (downloader-func 0 0 10000)))
          (displayln (format "~a downloaded" count)))
        (close-input-port port-in)
        (close-output-port port-out)
        filepath)))

  (define (current-webui-wire-link)
    (let* ((os (system-type 'os*))
           (arch (system-type 'arch))
           )
      (when (eq? os #f)
        (error "Operating system not automatically supported by webui-wire, you can compile it yourself and use 'ww-set-custom-webui-wire-command!'"))
      (let ((os-str (symbol->string os))
            (arch-str (symbol->string arch))
            (ext (if (eq? os 'linux)
                     ".flatpak"
                     (if (eq? os 'win64)
                         ".exe"
                         ""))))
        (string-append "https://github.com/hdijkema/webui-wire/releases/download/v"
                       ww-wire-version "/webui-wire-v" ww-wire-version
                       "-" os-str "-" arch-str ext)
        )
      )
    )


  )
