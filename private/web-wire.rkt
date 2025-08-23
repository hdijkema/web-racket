(module web-wire racket/base

  (require racket/system
           racket/file
           racket/gui
           racket/port
           file/unzip
           net/url
           racket/port
           data/queue
           json
           "../utils/utils.rkt"
           "css.rkt"
           html-printer
           )

  (provide ww-start
           ww-stop
           ww-set-debug
           
           ww-new
           ww-move
           ww-resize
           ww-set-title
           ww-set-icon
   )

  (define current-win-release "https://github.com/hdijkema/web-wire/releases/download/0.1/web-wire-0.1-win64.zip")

  (define (os)
    (format "~a-~a" (system-type) (system-type 'word)))

  (define (web-wire-exe)
    (if (eq? (system-type) 'windows)
        "web-wire.exe"
        "web-wire"))

  (define (web-wire-dir)
    (let* ((cache-dir (find-system-path 'cache-dir))
           (os-dir (build-path cache-dir (os))))
      os-dir))

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

  (define (download-if-needed release)
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some utils
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (drop expected s)
    (let ((e (string-append expected ":")))
      (if (string-prefix? s e)
          (substring s (string-length e))
          #f)))

  (define (as-string s)
    (with-output-to-string (lambda () (write s))))

  (define (to-server-file html-file)
    (let* ((path (build-path html-file))
           (complete-p (path->complete-path path))
           (a-file (format "~a" complete-p))
           (the-file (string-replace a-file "\\" "/")))
      the-file))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; web-wire handling
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define ww-err-thread #f)
  (define ww-to-ww #f)
  (define ww-from-ww #f)
  (define ww-quit #f)

  (define ww-debug #f)
  
  (define (ww-set-debug yn) (set! ww-debug yn))

  (define (do-debug str . var)
    (when ww-debug
      (if (null? var)
          (displayln (format "Debug: ~a" str))
          (displayln (format "Debug: ~a: ~a" var str))
          )))

  (define (err str)
    (displayln (format "Error: ~a" str)))

  (define-syntax debug
    (syntax-rules ()
      ((_ str)
       (do-debug str))
      ((_ var str)
       (do-debug str 'var))
      ))
  
  (define re-kind #px"([A-Z]+)[(]([0-9]+)[)][:]")
  (define re-event #px"([^:]+)[:]([0-9]+)([:](.*))?")
  (define re-js-event #px"^([^:]+)([:](.*))?")
  (define re-js-result #px"([0-9]+)[:]([^:]+)[:](.*)")
  (define re-js-handle #px"([^:]+)[:]([0-9]+)[:]([0-9]+)([:](.*))?")

  (define windows-evt-handlers (make-hash))
  (define windows (make-hash))

  (define handle-results (make-hash))
  (define handle-semaphores (make-hash))

  (define (ww-get-js-handle r)
    (let* ((h-info (cdr r))
           (m (regexp-match re-js-handle h-info)))
      (if (eq? m #f)
          #f
          (let* ((kind (string->symbol (cadr m)))
                 (win (string->number (caddr m)))
                 (handle (string->number (cadddr m)))
                 (rest (car (cddddr m)))
                 )
            handle))))

  (define (ww-start)

    (define protocol-version 0)

    (define (handle-event line)
      (debug (format "Handling ~a" line))
      (let ((m (regexp-match re-event line)))
        (if (eq? m #f)
            (err (format "Cannot interpret input: ~a" line))
            (let* ((str-evt (cadr m))
                   (evt (string->symbol str-evt))
                   (win-id (string->number (caddr m)))
                   (content (car (cddddr m)))
                   (win* (hash-ref windows-evt-handlers win-id #f))
                   (win (if (eq? win* #f) #f (weak-box-value win*)))
                   )
              ;(debug content content)
              (unless (or (eq? evt 'closed) (eq? evt 'js-result))
                (if (eq? win #f)
                    (begin
                      (err (format "No such window ~a" win-id))
                      (err (format "Cannot handle event '~a " evt))
                      (err (format "input: ~a" line))
                      )
                    (if (string-prefix? str-evt "js-")
                        (let* ((evt (string->symbol (substring str-evt 3)))
                               (m (regexp-match re-js-event content))
                               (element-id (string->symbol (cadr m)))
                               (content (string-trim (cadddr m)))
                               (h  (with-input-from-string content read-json))
                               (data (if (string=? content "")
                                         ""
                                         (hash-ref h 'data #f)))
                               )
                          (queue-callback (lambda ()
                                            (win 'js evt (list element-id data)))))
                        (queue-callback (lambda () (win 'other evt content))))
                    ))
              (when (eq? evt 'js-result)
                (let ((m (regexp-match re-js-result content)))
                  (if (eq? m #f)
                      (err (format "Cannot interpret js-result: ~a" content))
                      (let* ((handle (string->number (cadr m)))
                             (func (caddr m))
                             (content (cadddr m))
                             (data (with-input-from-string content read-json))
                             (result (hash-ref data 'result))
                             )
                        (if (hash-has-key? handle-results handle)
                            (begin
                              ; a result is expected
                              (hash-set! handle-results handle result)
                              (semaphore-post (hash-ref handle-semaphores handle)))
                            (debug (format "not awaiting ~a: ~a" handle content)))
                        )
                      )))
              (when (eq? evt 'closed)
                (if (eq? win #f)
                    (err (format "No such window ~a, cannot close" win-id))
                    (hash-remove! windows-evt-handlers win-id)))
              ))
        ))

    (define (web-wire-err-handler err-ww)
      (let* ((line-in (read-line err-ww))
             (go-on   #t))
        (while (and (not ww-quit) (not (eof-object? line-in)))
           (let* ((line (string-trim line-in))
                  (m (regexp-match re-kind line))
                  )
             (if (eq? m #f)
                 (debug line)
                 (let* ((kind (string->symbol (cadr m)))
                        (lines (string->number (caddr m)))
                        (rest (substring line (string-length (car m))))
                        (more-lines (- lines 1))
                        )
                   (while (> more-lines 0)
                       (let ((line (string-trim (read-line err-ww))))
                         (set! rest (string-append rest "\n" line))
                         (set! more-lines (- more-lines 1))
                         ))
                   (cond
                     ([eq? kind 'EVENT]
                      (with-handlers ([exn:fail?
                                       (lambda (e) (err (format "~a" e)))])
                                      (handle-event rest))
                      )
                     (else (debug (format "~a(~a):~a" kind lines rest)))
                     ))
                 ))
               (set! line-in (read-line err-ww))))
      #t)

    (define (web-wire-proc-control p-c from-ww to-ww err-ww)
      (let ((status (p-c 'status)))
        (while (eq? status 'running)
               (sleep 1)
               (set! status (p-c 'status)))
        (p-c 'wait)
        (close-input-port from-ww)
        (close-output-port to-ww)
        (close-input-port err-ww)
        (set! ww-to-ww #f)
        (set! ww-from-ww #f)
        (set! ww-err-thread #f)
        (set! ww-quit #f)
      ))
    
    (if (eq? ww-err-thread #f)
        (begin
          ;; Maybe we need to download the web-wire executable
          (let* ((os (system-type))
                 (release (cond
                            ([eq? os 'windows] current-win-release)
                            (else #f))))
            (unless (eq? release #f)
              (download-if-needed release)))

          ;; Start the web-wire process for errors and events in an other thread
          (let ((cwd (current-directory)))
            (current-directory (web-wire-dir))
            (let ((ports (process (web-wire-exe))))
              (current-directory cwd)
              (let ((from-ww (car ports))
                    (to-ww (cadr ports))
                    (ww-pid (caddr ports))
                    (err-from-ww (cadddr ports))
                    (proc-control (car (cddddr ports)))
                    )
                
                (set! ww-to-ww to-ww)
                (set! ww-from-ww from-ww)
                (set! ww-quit #f)
                
                (parameterize ([current-eventspace (current-eventspace)])
                  (set! ww-err-thread
                        (thread (lambda () (web-wire-err-handler err-from-ww))))
                  (thread (lambda () (web-wire-proc-control proc-control
                                                            from-ww
                                                            to-ww err-from-ww)))
                  )
                ww-pid
                )))
          )
        #f))

  (define current-handle 0)

  (define (new-handle)
    (set! current-handle (+ current-handle 1))
    current-handle)

  (define (ww-cmd cmd)
    (if (eq? cmd 'quit)
        (begin
          (displayln "exit" ww-to-ww)
          (flush-output ww-to-ww)
          (set! ww-quit #t))
        (begin
          (displayln cmd ww-to-ww)
          (flush-output ww-to-ww))
        )
    (let ((line-in (string-trim (read-line ww-from-ww))))
      (let ((ok (string-prefix? line-in "OK("))
            (nok (string-prefix? line-in "NOK("))
            )
        (let ((m (regexp-match re-kind line-in)))
          (unless m
            (error (format "Input not expected: ~a" line-in)))
          (let* ((kind (cadr m))
                 (lines (string->number (caddr m)))
                 (result-str (substring line-in (string-length (car m))))
                 (more-lines (- lines 1))
                 )
            ;(displayln result-str)
            ;(displayln (format "~a ~a ~a" kind lines more-lines))
            (while (> more-lines 0)
                   (set! result-str (string-append
                                     result-str "\n"
                                     (string-trim (read-line ww-from-ww))))
                   (set! more-lines (- more-lines 1)))
            (cons ok result-str)
            ))))
    )

  (define (ww-await handle cmd)
    (hash-set! handle-semaphores handle (make-semaphore 0))
    (hash-set! handle-results handle #f)
    (let* ((r (ww-cmd cmd))
           (result (car r))
           (content (cdr r))
           )
      (if r
          (begin
            (semaphore-wait (hash-ref handle-semaphores handle))
            (hash-remove! handle-semaphores handle)
            (let ((r (hash-ref handle-results handle)))
              (hash-remove! handle-results handle)
              r))
          (begin
            (hash-remove! handle-semaphores handle)
            (hash-remove! handle-results handle)
            #f)
          )
      )
    )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web Wire Commands
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Stop the QtWebEngine
  (define (ww-stop)
    (let ((win-ids (hash-keys windows-evt-handlers)))
      (for-each (λ (win-id)
                  (ww-close win-id))
                win-ids))
    (let ((r (ww-cmd 'quit)))
      (car r)))

  ;; Debug window
  (define (ww-devtools win-id)
    (let ((cmd (format "debug ~a" win-id)))
      (ww-cmd cmd)))
  
  ;; New window 
  (define (ww-new profile . parent)
    (let* ((parent-win-id (if (null? parent)
                              ""
                              (if (eq? (car parent) #f)
                                  ""
                                  (format " ~a" (car parent)))))
           (cmd (string-append "new " (format "~a" profile) parent-win-id))
           (r (ww-cmd cmd)))
      (let ((ok (car r))
            (res (cdr r)))
        (if ok
            (let* ((win-str-id (drop "new" res))
                   (win-id (if (eq? win-str-id #f) #f (string->number win-str-id)))
                   )
              win-id)
            #f))))

  ;; Close window
  (define (ww-close win-id)
    (let ((r (ww-cmd (format "close ~a" win-id))))
      (car r)))

  ;; Move window
  (define (ww-move win-id x y)
    (let ((r (ww-cmd (format "move ~a ~a ~a" win-id x y))))
      (car r)))

  ;; Resize window
  (define (ww-resize win-id width height)
    (let ((r (ww-cmd (format "resize ~a ~a ~a" win-id width height))))
      (car r)))

  ;; Set title of window
  (define (ww-set-title win-id title)
    (let ((r (ww-cmd (format "set-title ~a ~a" win-id (as-string title)))))
      (car r)))


  ;; Set icon of window
  (define (ww-set-icon win-id file)
    (if (file-exists? file)
        (let* ((icon-file (to-server-file file))
               (cmd (format "set-icon ~a ~a"
                                    win-id (as-string (format "~a" icon-file))))
               (r (ww-cmd cmd)))
          (car r))
        (error "ww-set-icon - file does not exist")))

  ;; Set menu of window
  (define (ww-set-menu win-id menu-list)

    (define (atom? e) (not (pair? e)))

    (define (is-simple-entry-form? e)
      (if (= (length e) 2)
          (if (atom? (car e))
              (let ((r (cdr e)))
                (if (atom? r)
                    #t
                    (if (list? r)
                        (if (= (length r) 1)
                            (if (atom? (car r))
                                #t
                                (if (list? (car r))
                                    (if (null? (car r))
                                        #f
                                        (atom? (caar r)))
                                    (atom? (car r)))
                                )
                            #f)
                        #f)
                    )
                )
              #f)
          #f)
      )
    
    (define (is-simple-entry? e)
      (if (= (length e) 2)
          (if (string? (car e))
              (let ((r (cdr e)))
                (if (symbol? r)
                    #t
                    (if (list? r)
                        (if (= (length r) 1)
                            (if (symbol? (car r))
                                #t
                                (if (list? (car r))
                                    (if (null? (car r))
                                        #f
                                        (symbol? (caar r)))
                                    (symbol? (car r))))
                            #f)
                        #f)
                    ))
              #f)
          #f))

    ; Pre: (is-simple-entry? e)
    (define (cvt-simple-entry e)
      (let ((s (car e))
            (r (cdr e)))
            (if (symbol? r)
                (list s r)
                (if (symbol? (car r))
                    (list s (car r))
                    (list s (caar r)))
                ))
      )

    (define (pair->list p)
      (if (pair? p)
          (let* ((rest (cdr p))
                 (s (car p))
                 (r (list s rest)))
            (if (is-simple-entry? r)
                (cvt-simple-entry r)
                (if (list? (car rest))
                    (if (is-simple-entry? (car rest))
                        r
                        (list s (car rest)))
                    r)))
          p))

    (define (is-submenu? e)
      (if (= (length e) 2)
          (not (is-simple-entry? e))
          #t))

    (define (cvt-submenu e)
      (if (list? e)
          (if (= (length e) 1)
              (if (is-simple-entry? (car e))
                  e
                  (car e))
              e)
          e))

    (define (cvt-menu-list l)
      (map (lambda (e)
             ;(displayln e)
             (unless (or (pair? e) (list? e))
               (error "Unexpected item: ~a" e))
             (let ((e* (pair->list e)))
               (if (is-simple-entry? e*)
                   (cvt-simple-entry e*)
                   (if (is-simple-entry-form? e*)
                       (error (format "Menu entry must be '(title: string id:symbol), got ~a" e))
                       (if (is-submenu? e*)
                           (list (car e*) (cvt-menu-list (cvt-submenu (cdr e*))))
                           (error (format "Unknown menu entry: ~a" e))
                           )))))
           l))

    (define (to-json-menu l)
      (map (lambda (e)
             (if (is-simple-entry? e)
                 (list (car e) (symbol->string (cadr e)))
                 (list (car e) (to-json-menu (cadr e)))))
           l))
                       
    (unless (list? menu-list)
      (error "A menu list must contain menu entries (pairs/lists of string + symbol/submenu lists)"))
    (let ((ml (cvt-menu-list menu-list)))
      (let ((jml (to-json-menu ml)))
        (let* ((json-menu (with-output-to-string (lambda () (write-json jml))))
               (as-str (with-output-to-string (lambda () (write json-menu))))
               (cmd (format "set-menu ~a ~a" win-id as-str)))
          (let ((r (ww-cmd cmd)))
            (car r)))))
    )



  ;; Set html of window
  (define (ww-set-html win-id html-file)
    (if (file-exists? html-file)
        (let ((cmd (format "set-html ~a ~a ~a"
                           win-id (new-handle)
                           (as-string (to-server-file html-file)))))
          (let ((r (ww-cmd cmd)))
            (car r)))
        (error "set-html: file does not exist")
        ))

  ;; Set inner html of an Id of the HTML in the window
  (define (ww-set-inner-html win-id element-id html-or-file)
    (if (file-exists? html-or-file)
        (let* ((js-handle (new-handle))
               (cmd (format "set-inner-html ~a ~a ~a ~a"
                            win-id js-handle
                            (format "~a" element-id)
                            (as-string (to-server-file html-or-file))))
               )
          (ww-await js-handle cmd))
        (let* ((js-handle (new-handle))
               (cmd (format "set-inner-html ~a ~a ~a ~a"
                            win-id js-handle
                            (format "~a" element-id)
                            (as-string (format "~a" html-or-file))))
               )
          (ww-await js-handle cmd))
        ))

  ;; Het the inner html of an id of the HTML in the window
  (define (ww-get-inner-html win-id element-id)
    (let* ((js-handle (new-handle))
           (cmd (format "get-inner-html ~a ~a ~a"
                        win-id js-handle (format "~a" element-id)))
           )
      (ww-await js-handle cmd)))

  ;; Set attribute of element in html
  (define (ww-set-attr win-id element-id attr val)
    (let* ((js-handle (new-handle))
           (cmd (format "set-attr ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string val))))
      (ww-cmd cmd)))

  ;; Get attribute value of element in html
  (define (ww-get-attr win-id element-id attr)
    (let* ((js-handle (new-handle))
           (cmd (format "get-attr ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string attr))))
      (ww-await js-handle cmd)))

  ;; Delete attribute of element
  (define (ww-del-attr win-id element-id attr)
    (let* ((js-handle (new-handle))
           (cmd (format "del-attr ~a ~a ~a" win-id js-handle
                        (as-string element-id))))
      (ww-cmd cmd)))

  ;; get value of an element
  (define (ww-get-value win-id element-id)
    (let* ((js-handle (new-handle))
           (cmd (format "value ~a ~a ~a" win-id js-handle
                        (as-string element-id))))
      (ww-await js-handle cmd)))

  ;; set value of an element
  (define (ww-set-value win-id element-id val)
    (let* ((js-handle (new-handle))
           (cmd (format "value ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id)
                        (as-string val))))
      (ww-await js-handle cmd)))
  

  ;; Bind some CSS selector to an event, given that each
  ;; element that satisfies the selector has to have an id.
  ;; Returns the element ids as symbols
  (define (ww-bind win-id event selector)
    (let* ((js-handle (new-handle))
           (cmd (format "bind ~a ~a ~a ~a" win-id js-handle
                       (as-string event) (as-string selector))))
      (map string->symbol (ww-await js-handle cmd))))

  ;; Add a class to an element
  (define (ww-add-class win-id element-id class)
    (let* ((js-handle (new-handle))
           (cmd (format "add-class ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string class)))
           )
      (ww-cmd cmd)))

  ;; Remove a class from an element
  (define (ww-remove-class win-id element-id class)
    (let* ((js-handle (new-handle))
           (cmd (format "remove-class ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string class)))
           )
      (ww-cmd cmd)))

  ;; Has a class
  (define re-class-split #px"\\s+")
  
  (define (ww-has-class? win-id element-id class*)
    (let* ((cl (string-trim (ww-get-attr win-id element-id "class")))
           (class (format "~a" class*)))
      (if (eq? cl #f)
          #f
          (let* ((cls (regexp-split re-class-split cl)))
            (letrec ((f (lambda (cls)
                          (if (null? cls)
                              #f
                              (let ((cl (car cls)))
                                (if (string=? cl class)
                                    #t
                                    (f (cdr cls))))
                              ))
                        ))
              (f cls)))
          )
      )
    )

  ;; Add a style to an element
  (define (ww-add-style win-id element-id css-style)
    (let* ((st (css-style->string css-style))
           (js-handle (new-handle))
           (cmd (format "add-style ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string st)))
           )
      (ww-cmd cmd)))

  ;; Set a style of an element
  (define (ww-set-style win-id element-id css-style)
    (let* ((st (css-style->string css-style))
           (js-handle (new-handle))
           (cmd (format "set-style ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string st)))
           )
      (ww-cmd cmd)))

  ;; Get the style of an element
  (define (ww-get-style win-id element-id)
    (let* ((js-handle (new-handle))
           (cmd (format "get-style ~a ~a ~a" win-id js-handle
                       (as-string element-id)))
           )
      (string->css-style (ww-await js-handle cmd))))
                      
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Finalizing stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define will (make-will-executor))

  (define (register-finalizer obj proc)
    (will-register will obj proc))

  (void (thread (λ () (let loop () (will-execute will) (loop)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Regexes
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define re-resize #px"([0-9]+)\\s+([0-9]+)")
  (define re-move re-resize)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GUI classes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define _std_x 100)
  (define _std_y 100)
  (define _std_w 800)
  (define _std_h 600)

  (define (next-window-init-position)
    (set! _std_x (+ _std_x 75))
    (set! _std_y (+ _std_y 50))
    (call-with-values
     get-display-size
     (lambda (w h)
       (when (> (+ _std_y _std_h) h)
         (set! _std_y 50))
       (when (> (+ _std_x _std_w) w)
         (set! _std_x 50))
       )))

  (define ww-element%
    (class object%
      (init-field [win-id #f] [id #f])
      
      (define/public (get-win-id)
        win-id)

      (define/public (get-id)
        id)

      (define/public (win)
        (let ((w (hash-ref windows win-id #f)))
          w))

      (define connected-callbacks (make-hash))

      (define/public (callback evt . args)
        (let ((cb (hash-ref connected-callbacks evt #f)))
          (unless (eq? cb #f)
            (with-handlers ([exn:fail?
                             (λ (e)
                               (err (format "callback for ~a: ~a" evt e)))])
              (apply cb args)))))

      (define/public (connect evt func)
        (hash-set! connected-callbacks evt func))

      (define/public (disconnect evt)
        (hash-remove! connected-callbacks evt))

      (define/public (add-style! st)
        (ww-add-style win-id id st))

      (define/public (set-style! st)
        (ww-set-style win-id id st))

      (define/public (style)
        (ww-get-style win-id id))

      (define/public (add-class! cl)
        (ww-add-class win-id id cl))

      (define/public (remove-class! cl)
        (ww-remove-class win-id id cl))

      (define/public (has-class? cl)
        (ww-has-class? win-id id cl))

      (define/public (enable)
        (send this remove-class! 'disabled))

      (define/public (enabled?)
        (not (send this disabled?)))

      (define/public (disable)
        (send this add-class! 'disabled))

      (define/public (disabled?)
        (send this has-class? 'disabled))

      (define/public (display . args)
        (let ((d (if (null? args) "block" (car args))))
          (send this add-style! (css-style 'display d))))

      (define/public (hide)
        (send this display "none"))

      (define/public (show)
        (send this display "block"))

      (define/public (show-inline)
        (send this display "inline-block"))

      (define/public (set-inner-html html-or-sexpr)
        (if (string? html-or-sexpr)
            (ww-set-inner-html win-id id html-or-sexpr)
            (set-inner-html (xexpr->html5 html-or-sexpr))))

      (super-new)
      )
    )

  (define ww-input%
    (class ww-element%

      (define/public (get)
        (ww-get-value (send this get-win-id) (send this get-id)))

      (define/public (set! v)
        (ww-set-value (send this get-win-id) (send this get-id) v))
          
      (super-new)))

  (define ww-window%
    (class object%
      
      (init-field [profile 'default-profile]
                  [parent-id #f]
                  [title "Racket HTML Window"]
                  [x _std_x]
                  [y _std_y]
                  [width _std_w]
                  [height _std_h]
                  [icon #f]
                  [menu #f]
                  [html-file #f]
                  )

      (define win-id #f)
      
      (define elements (make-hash))
      
      (define (event-handler type evt content)
        (displayln (format "win-id=~a '~a '~a ~a" win-id type evt content))
        (cond
          ([eq? evt 'page-loaded] (begin
                                    (send this bind-buttons)
                                    (send this bind-inputs)))
          ([eq? evt 'click] (handle-click (car content) (cadr content)))
          ([eq? evt 'change] (handle-change (car content) (cadr content)))
          ([eq? evt 'resized] (let* ((m (regexp-match re-resize content))
                                     (width* (string->number (cadr m)))
                                     (height* (string->number (caddr m)))
                                     )
                                (set! width width*)
                                (set! height height*)))
          ([eq? evt 'moved] (let* ((m (regexp-match re-move content))
                                   (x* (string->number (cadr m)))
                                   (y* (string->number (caddr m)))
                                   )
                              (set! x x*)
                              (set! y y*)
                              ))
          )
        )

      (define/public (handle-click element-id data)
        (let ((el (hash-ref elements element-id #f)))
          (unless (eq? el #f)
            (send el callback 'click data))))

      (define/public (handle-change element-id data)
        (let ((el (hash-ref elements element-id #f)))
          (unless (eq? el #f)
            (send el callback 'change (hash-ref data 'value)))))

      (define/public (get-win-id) win-id)

      (define/public (bind event selector cl)
        (let ((ids (ww-bind win-id event selector)))
          (for-each (λ (id)
                      (hash-set! elements id
                                 (new cl [win-id win-id] [id id])))
                    ids)))

      (define/public (bind-inputs)
        (bind 'change 'input ww-input%)
        (bind 'change 'textarea ww-input%)
        )

      (define/public (bind-buttons)
        (bind 'click 'button ww-element%)
        )

      (define/public (element id)
        (let ((el (hash-ref elements id #f)))
          (if (eq? el #f)
              (begin
                (hash-set! elements id (new ww-element%
                                            [win-id win-id] [id id]))
                (element id))
              el)))

      (define/public (move x y)
        (ww-move win-id x y))

      (define/public (resize x y)
        (ww-resize win-id x y))

      (define/public (get-x) x)
      (define/public (get-y) y)
      (define/public (get-width) width)
      (define/public (get-height) height)
      (define/public (geom) (list x y width height))
      

      (define/public (set-title! t)
        (set! title t)
        (ww-set-title win-id t))

      (define/public (get-title)
        title)

      (define/public (set-html-file! file)
        (set! html-file file)
        (ww-set-html win-id html-file))

      (define/public (get-html-file)
        html-file)

      ; construct
      (begin
        ;(displayln (format "profile: ~a, ~a" profile parent-id))
        (next-window-init-position)
        
        (set! win-id (ww-new profile parent-id))
        (when (eq? win-id #f)
          (error "Window could not be constructed"))

        (hash-set! windows-evt-handlers win-id (make-weak-box event-handler))
        (hash-set! windows win-id (make-weak-box this))

        (ww-move win-id x y)
        (ww-resize win-id width height)
        
        (ww-set-title win-id title)
        
        (unless (eq? icon #f)
          (ww-set-icon win-id icon))
        
        (unless (eq? menu #f)
          (ww-set-menu win-id menu))
        
        (unless (eq? html-file #f)
          (ww-set-html win-id html-file))

        (register-finalizer this
                            (λ (me)
                              (let ((win-id (send me get-win-id)))
                                (ww-close (send me get-win-id))
                                (hash-remove! windows win-id)
                                (hash-remove! windows-evt-handlers win-id)
                                )))
        )

      (super-new)
    ))

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
      (super-new [html-file "../../web-wire/test/test1.html"])))

  
  ); end of module