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
           )

  (provide ww-start
           ww-stop
           
           ww-set-debug
           ww-debug
           ww-error
           ww-devtools

           ww-cmd
           ww-await

           ww-set-stylesheet
           ww-get-stylesheet
           
           ww-new
           ww-close
           
           ww-move
           ww-resize
           ww-set-title
           ww-set-icon

           ww-set-menu

           ww-set-html
           ww-set-url
           
           ww-set-inner-html
           ww-get-inner-html

           ww-set-attr
           ww-get-attr
           ww-get-attrs
           ww-del-attr
           
           ww-set-style
           ww-add-style
           ww-get-style
           
           ww-add-class
           ww-remove-class
           ww-has-class?
           
           ww-set-value
           ww-get-value

           ww-get-elements

           ww-set-show-state
           ww-show-state

           ww-bind
           ww-on
           ww-element-info

           ww-file-open
           ww-file-save
           ww-choose-dir

           windows
           windows-evt-handlers
           ww-get-window-for-id

           ww-from-string
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
    (let ((s* (format "~a" s)))
      (with-output-to-string (lambda () (write s*)))))

  (define (ww-from-string s)
    (let ((s* (substring s 1 (- (string-length s) 1))))
      (string-replace s* "\\\"" "\"")))

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

  (define _ww-debug #f)
  
  (define (ww-set-debug yn) (set! _ww-debug yn))

  (define (do-debug str . var)
    (when _ww-debug
      (if (null? var)
          (displayln (format "Debug: ~a" str))
          (displayln (format "Debug: ~a: ~a" (car var) str))
          )))

  (define (err str . var)
    (if (null? var)
        (displayln (format "Error: ~a" str))
        (displayln (format "Error: ~a: ~a" var str))
        ))

  (define-syntax debug
    (syntax-rules ()
      ((_ str)
       (do-debug str))
      ((_ var str)
       (do-debug str 'var))
      ))

  (define-syntax ww-debug
    (syntax-rules ()
      ((_ str)
       (do-debug str))
      ((_ var str)
       (do-debug str 'var))
      ))

  (define-syntax ww-error
    (syntax-rules ()
      ((_ str)
       (err str))
      ((_ var str)
       (err str 'var))
      ))
  
  (define re-kind #px"([A-Z]+)[(]([0-9]+)[)][:]")
  (define re-event #px"([^:]+)[:]([0-9]+)([:](.*))?")
  (define re-js-event #px"^([^:]+)([:](.*))?")
  (define re-js-result #px"([0-9]+)[:]([^:]+)[:](.*)")
  (define re-js-handle #px"([^:]+)[:]([0-9]+)[:]([0-9]+)([:](.*))?")

  (define windows-evt-handlers (make-hash))
  (define windows (make-hash))

  (define (ww-get-window-for-id win-id)
    (hash-ref windows win-id #f))

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
    (ww-debug "ww-start called")

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
                   (win (hash-ref windows-evt-handlers win-id #f))
                   )
              ;(debug content content)
              (unless (or (eq? evt 'closed) (eq? evt 'js-result))
                (if (eq? win #f)
                    (unless (or (eq? evt 'show-event)
                                (eq? evt 'hide-event))
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
                    (begin
                      (hash-remove! windows-evt-handlers win-id)
                      (hash-remove! windows win-id))))
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
                       (let* ((line* (read-line err-ww))
                              (line (if (eof-object? line*)
                                        ""
                                        (string-trim line*))))
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

  (define (do-cmd cmd)
    (if (eq? ww-to-ww #f)
        (ww-error
         (format "Unexpected: (eq? ww-to-ww  #f), for command '~a'" cmd))
        (begin
          (displayln cmd ww-to-ww)
          (flush-output ww-to-ww))
        )
    )
      

  (define (ww-cmd cmd)
    (if (eq? cmd 'quit)
        (begin
          (do-cmd "exit")
          (set! ww-quit #t))
        (begin
          (do-cmd cmd))
        )
    (if (eq? ww-from-ww #f)
        (begin
          (ww-error
           (format "Unexpected: (eq? ww-from-ww #f), for command '~a" cmd))
          (cons #f 'nil))
        (let* ((line-in* (read-line ww-from-ww))
               (line-in (if (eof-object? line-in*)
                            "NOK(1):eof:0"
                            (string-trim line-in*)))
               )
          (let ((ok (string-prefix? line-in "OK("))
                (nok (or (string=? line-in "")
                         (string-prefix? line-in "NOK(")))
                )
            (let ((m (regexp-match re-kind line-in)))
              (unless m
                (ww-debug (format "Input not expected: \"~a\", maybe ww-quit issued" line-in)))
              (let* ((kind (cadr m))
                     (lines (string->number (caddr m)))
                     (result-str (substring line-in (string-length (car m))))
                     (more-lines (- lines 1))
                     )
                ;(displayln result-str)
                ;(displayln (format "~a ~a ~a" kind lines more-lines))
                (let ((rdln (λ ()
                              (let ((l (read-line ww-from-ww)))
                                (if (eof-object? l)
                                    ""
                                    (string-trim l))))))
                  (while (> more-lines 0)
                         (set! result-str (string-append
                                           result-str "\n"
                                           (rdln)))
                         (set! more-lines (- more-lines 1)))
                  )
                (cons ok result-str)
                ))))
        )
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
    (ww-debug "ww-stop called")
    (let ((win-ids (hash-keys windows-evt-handlers)))
      (for-each (λ (win-id)
                  (ww-close win-id))
                win-ids))
    (let ((r (ww-cmd 'quit)))
      (car r)))

  ;; Global stylesheet
  (define (ww-set-stylesheet st)
    (let* ((css (if (stylesheet? st)
                    (stylesheet->string st)
                    st))
           (h (let ((h (make-hasheq)))
                (hash-set! h 'css css)
                h))
           (json (jsexpr->string h))
           (cmd (format "set-stylesheet ~a" json))
           )
      (ww-cmd cmd)))

  (define (ww-get-stylesheet)
    (let ((cmd (format "get-stylesheet")))
      (let ((r (ww-cmd cmd)))
        (displayln r)
        #t)))
  
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


  ;; set url
  (define (ww-set-url win-id url)
    (let ((cmd (format "set-url ~a ~a ~a"
                       win-id (new-handle) (as-string url))))
      (ww-cmd cmd)))
  

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
           (cmd (format "set-attr ~a ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string attr) (as-string val))))
      (displayln cmd)
      (ww-cmd cmd)))

  ;; Get attribute value of element in html
  (define (ww-get-attr win-id element-id attr)
    (let* ((js-handle (new-handle))
           (cmd (format "get-attr ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id) (as-string attr))))
      (ww-await js-handle cmd)))

  
  ;; Get all attributes of an element with given id
  (define (mk-attrs _attrs)
      (let* ((attrs (make-hash)))
        (for-each (λ (attr-val)
                    (hash-set! attrs
                               (string->symbol (car attr-val))
                               (cadr attr-val)))
                  _attrs)
        attrs)
    )
  
  (define (ww-get-attrs win-id element-id)
    (let* ((js-handle (new-handle))
           (cmd (format "get-attrs ~a ~a ~a" win-id js-handle
                        (as-string element-id))))
      (mk-attrs (ww-await js-handle cmd))))

  ;; Get info of all elements for a selector
  (define (ww-get-elements win-id selector)
    (let* ((js-handle (new-handle))
           (cmd (format "get-elements ~a ~a ~a" win-id js-handle
                        (as-string selector))))
      (map (λ (item)
             (cons (string->symbol (car item))
                   (mk-attrs (cadr item)))
             )
           (ww-await js-handle cmd))))

  ;; Delete attribute of element
  (define (ww-del-attr win-id element-id attr)
    (let* ((js-handle (new-handle))
           (cmd (format "del-attr ~a ~a ~a ~a" win-id js-handle
                        (as-string element-id)
                        (as-string attr))))
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
      (map (lambda (info)
             (map string->symbol info))
             (ww-await js-handle cmd))))

  (define (ww-on win-id event id)
    (let* ((js-handle (new-handle))
           (cmd (format "on ~a ~a ~a ~a"
                        win-id js-handle
                        (as-string event)
                        (as-string id))))
      (ww-cmd cmd)))

  ;; Element info
  (define (ww-element-info win-id id)
    (let* ((js-handle (new-handle))
           (cmd (format "element-info ~a ~a ~a" win-id js-handle
                        (as-string id))))
      (let ((result (ww-await js-handle cmd)))
        (list (if (symbol? id)
                  (string->symbol (car result))
                  (car result))
              (string->symbol (cadr result))
              (string->symbol (caddr result))
              (cadddr result)))))

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


  ;; Show State
  (define (ww-set-show-state win-id state)
    (let ((cmd (format "set-show-state ~a ~a" win-id (as-string state))))
      (ww-cmd cmd)))

  (define (ww-show-state win-id)
    (let ((cmd (format "show-state ~a" win-id)))
      (ww-cmd cmd)))

  ;; Files and directories
  (define (ww-file-open win-id title dir file-filters)
    (let ((cmd (format "file-open ~a ~a ~a ~a" win-id
                       (as-string title)
                       (as-string dir)
                       (as-string file-filters))))
      (ww-cmd cmd)))

  (define (ww-file-save win-id title dir file-filters overwrite)
    (let ((cmd (format "file-save ~a ~a ~a ~a ~a" win-id
                       (as-string title)
                       (as-string dir)
                       (as-string file-filters)
                       (if overwrite 1 0))))
      (ww-cmd cmd)))

  (define (ww-choose-dir win-id title dir)
    (let ((cmd (format "choose-dir ~a ~a ~a" win-id
                       (as-string title)
                       (as-string dir))))
      (ww-cmd cmd)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Finalizing stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;(define will (make-will-executor))

  ;(define (ww-register-finalizer obj proc)
  ;  (will-register will obj proc))

  ;(void (thread (λ () (let loop () (will-execute will) (loop)))))
  
  ); end of module