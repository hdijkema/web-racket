(module web-wire racket/base

  (require racket/system
           racket/file
           racket/gui
           racket/port
           data/queue
           json
           "../utils/utils.rkt"
           "css.rkt"
           "menu.rkt"
           "webui-wire-ipc.rkt"
           "webui-wire-download.rkt"
           )

  (provide ww-start
           ww-stop

           ww-set-debug
           ww-debug
           ww-error
           ww-display-log
           ww-tail-log
           ww-set-log-lines!

           ww-cmd
           ww-cmd-nok?

           ww-cwd

           ww-protocol
           ww-log-level
           ww-set-stylesheet
           ww-get-stylesheet
           
           ww-new
           ww-close
           
           ww-move
           ww-resize
           ww-set-title
           ww-set-icon

           ww-set-menu

           ww-set-html-file
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
           ww-get-show-state

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

           ww-win-id
   )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some utils
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (drop expected s)
    (let ((e (string-append expected ":")))
      (if (string-prefix? s e)
          (substring s (string-length e))
          #f)))

  (define (as-string s)
    (format "~a" s))
    ;(let ((s* (format "~a" s)))
    ;  (with-output-to-string (lambda () (write s*)))))

  (define (ww-from-string s)
    (let ((s* (substring s 1 (- (string-length s) 1))))
      (string-replace s* "\\\"" "\"")))

  (define (to-server-file html-file)
    (let ((to-file (λ (p) (string-replace (format "~a" p) "\\" "/")))
          (file-path (build-path (format "~a" html-file))))
      (if (absolute-path? file-path)
          (to-file file-path)
          (let* ((cwd (ww-cwd))
                 (full-file (build-path cwd (format "~a" html-file))))
            (if (file-exists? full-file)
                (to-file html-file)
                (to-file (path->complete-path (build-path html-file)))
                )
            )
          )
      )
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; web-wire handling (interaction with the library)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define _ww-debug #t)
  
  (define (ww-set-debug yn) (set! _ww-debug yn))

  (define (do-debug str . var)
    (when _ww-debug
      (if (null? var)
          (process-log 'WW-DEBUG (format "~a" str))
          (process-log 'WW-DEBUG (format "~a: ~a" (car var) str))
          )))

  (define (err str . var)
    (if (null? var)
        (process-log 'WW-ERROR (format "~a" str))
        (process-log 'WW-ERROR (format "~a: ~a" var str))
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
  (define re-js-event #px"^([^:]+)([:](.*))?")
  (define re-js-result #px"([0-9]+)[:]([^:]+)[:](.*)")
  (define re-js-handle #px"([^:]+)[:]([0-9]+)[:]([0-9]+)([:](.*))?")

  (define windows-evt-handlers (make-hash))
  (define windows (make-hash))

  (define (ww-get-window-for-id win-id)
    (hash-ref windows win-id #f))

  (define-struct web-rkt
    ([handle #:mutable]
     [event-thread #:mutable]
     [stop-thread #:mutable]
     [reader-thread #:mutable]
     [type #:mutable]
     )
    #:transparent
    )

  (define ww-current-handle #f)

  (define evt-sem  (make-semaphore))
  (define evt-fifo (make-queue))
  (define log-fifo (make-queue))
  (define log-fifo-max 2500)

  (define (ww-set-log-lines! n)
    (set! log-fifo-max (if (< n 10) 10
                          (if (> n 10000) 10000 n)))
    (ensure-fifo)
    )

  (define re-event #px"^([^:]+)([:]([^:]+))?")

  (define (event-queuer-ipc evt)
    (enqueue! evt-fifo evt)
    (semaphore-post evt-sem))
  
  (define (event-queuer-ffi  evt)
    (let ((evt* (bytes->string/utf-8 evt)))
      (enqueue! evt-fifo evt*)
      (semaphore-post evt-sem)))

  (define (process-event h evt)
    (let* ((m (regexp-match re-event evt)))
      (ww-debug evt)
      (let* ((e (string->symbol (string-downcase (list-ref m 1))))
             (win-id (if (eq? (list-ref m 3) #f) #f (string->number (list-ref m 3))))
             (evt-handler (hash-ref windows-evt-handlers win-id #f))
             (payload* (substring evt (string-length (list-ref m 0))))
             (payload (if (string=? payload* "")
                          (make-hash)
                          (with-input-from-string (substring payload* 1) read-json)))
            )
        (if (eq? evt-handler #f)
            (ww-error (format "no event handler to handle event ~a" evt))
            (queue-callback (lambda () (evt-handler e payload))))
        )
      )
    )

  (define evt-hdlr 0)
  
  (define (event-handler h)
    (parameterize ([current-eventspace (current-eventspace)])
      (thread
       (lambda ()
         (letrec ((f (lambda ()
                       (semaphore-wait evt-sem)
                       (queue-callback
                        (lambda ()
                          (letrec ((queue-loop (λ ()
                                                 (when (> (queue-length evt-fifo) 0)
                                                   (process-event h (dequeue! evt-fifo))
                                                   (queue-loop)))))
                            (queue-loop))))
                       (f))))
           (f)))
       )
      )
    )

  (define (ensure-fifo)
    (if (> (queue-length log-fifo) log-fifo-max)
        (begin
          (dequeue! log-fifo)
          (ensure-fifo))
        (queue-length log-fifo)))

  (define tail-handler #f)

  (define (put-in-fifo kind msg)
    (enqueue! log-fifo (cons kind msg))
    (when tail-handler
      (tail-handler kind msg))
    )

  (define (process-log-ipc kind msg)
    (put-in-fifo kind msg)
    (ensure-fifo))

  (define process-log process-log-ipc)
  
  (define (ww-do-display item filter)
    (let ((displ #f))
      (when (eq? filter #f)
        (set! displ #t))
      (when (symbol? filter)
        (set! displ (eq? (car item) filter)))
      (when (regexp? filter)
        (when (or (regexp-match filter (cdr item))
                  (regexp-match filter (symbol->string (car item))))
          (set! displ #t)))
      (when displ
        (displayln (format "~a - ~a" (car item) (cdr item)))))
    )

  (define (ww-display-log . filter*)
    (let ((filter (if (null? filter*) #f (car filter*))))
      (when (string? filter)
        (set! filter (pregexp (string-append "(?i:" filter ")"))))
      (when (list? filter)
        (set! filter (pregexp (string-append "(?i:(" (string-join filter "|") "))"))))
      (for-each (λ (item)
                  (ww-do-display item filter))
                (queue->list log-fifo)))
    )

  (define (ww-tail-log . args)
    (let ((last-n 3)
          (filter #f)
          (stop-tail #f)
          )
      (let ((f (λ ()
                 (let ((arg (if (null? args) #f (car args))))
                   (unless (null? args)
                     (when (boolean? arg)
                       (when (eq? arg #f)
                         (set! stop-tail #t)))
                     (when (number? arg)
                       (set! last-n (if (< arg 0) 0 arg)))
                     (when (or (symbol? arg) (regexp? arg) (string? arg) (list? arg))
                       (set! filter arg)))
                   (set! args (if (null? args) '() (cdr args)))))))
        (f)(f))
      (ww-debug (format "tail log: ~a ~a ~a" last-n filter stop-tail))
      (if stop-tail
          (set! tail-handler #f)
          (let* ((l (queue->list log-fifo))
                 (len (length l)))
            (let ((nl (take-right l (if (<= last-n len) last-n len))))
              (when (string? filter)
                (set! filter (pregexp (string-append "(?i:" filter ")"))))
              (when (list? filter)
                (set! filter (pregexp (string-append "(?i:(" (string-join filter "|") "))"))))
              (for-each (λ (item)
                          (ww-do-display item filter))
                        nl))
            (set! tail-handler (λ (kind msg)
                                 (ww-do-display (cons kind msg) filter))))
          )
      )
    )

  (define (ww-start* type args)
    (when (eq? ww-current-handle #f)
      (set! evt-sem (make-semaphore))
      (set! evt-fifo (make-queue))
      (set! log-fifo (make-queue))
      (let ((h (make-web-rkt (if (eq? type 'ipc)
                                 (webui-ipc event-queuer-ipc process-log-ipc)
                                 (error "ffi integration not implemented"))
                             #f
                             #f
                             #f
                             type)))
        (when (eq? type 'ffi)
          (error "ffi integration not implemented"))
        (let ((thrd (event-handler h)))
          (when (eq? type 'ffi)
            (error "ffi integration not implemented"))
          (set-web-rkt-event-thread! h thrd)
          (set! ww-current-handle h))
        ))
    (unless (null? args)
      (ww-log-level (car args)))
    ww-current-handle)

  (define (ww-start . args)
    (ww-start* 'ipc args))

  (define (ww-stop)
    (unless (eq? ww-current-handle #f)
      ;; inform event handlers of destroying of windows.
      (let ((keys (hash-keys windows-evt-handlers)))
        (for-each (λ (win-id)
                    (let ((handler (hash-ref windows-evt-handlers win-id)))
                      (handler 'destroyed #f)))
                  keys))
      (with-handlers ([exn:fail? (λ (e) #t)])
        (ww-cmd "exit"))
      (let ((thr (web-rkt-event-thread ww-current-handle)))
        (kill-thread thr))
      (when (eq? (web-rkt-type ww-current-handle) 'ffi)
        (error "ffi integration not implemented"))
      (set! ww-current-handle #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Commands
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;; Types
  
  (define-struct ww-win
    (id)
    #:transparent)

  (define-struct cmdr
    (ok kind win r) #:transparent)

  ;;;;;;;;;;;; Command Utilities

  (define (cmdr->list r)
    (list (cmdr-ok r) (cmdr-kind r)
          (cmdr-win r) (cmdr-r r)))

  (define (cmdr-dbg r)
    (ww-debug (cmdr->list r))
    (cmdr-r r))

  (define (ww-from-json result)
    (hash-ref (with-input-from-string
                  (string-replace result "\\\"" "\"")
                read-json) 'result #f))

  (define re-generic-result #px"^(OK|NOK)[:]([^:]+)([:]([0-9]+))?")

  (define (convert-result result)
    (let ((m (regexp-match re-generic-result result)))
      (ww-debug result)
      (ww-debug m)
      (if m
          (let* ((ok (string=? (list-ref m 1) "OK"))
                 (kind (list-ref m 2))
                 (win (if (eq? (list-ref m 4) #f)
                          #f
                          (string->number (list-ref m 4))))
                 (r (substring result (string-length (list-ref m 0))))
                 )
            (make-cmdr ok (string->symbol kind)
                       win
                       (if (string-prefix? r ":")
                           (substring r 1)
                           r)))
          (make-cmdr #f 'parse-error #f result))
      )
    )

  (define (check-nok cmd r)
    (when (eq? (cmdr-ok r) #f)
      (process-log 'CMD-NOK
                   (format "~a - ~a" cmd (cmdr->list r)))))

  ;;;;;;;;;;;; Calling commands in the web wire environment
  
  (define (ww-cmd cmd)
    (ww-debug (format "ww-cmd ~a" cmd))
    (if (eq? cmd 'quit)
        (begin
          (ww-stop)
          #t)
        (let* ((type (web-rkt-type ww-current-handle))
               (result (if (eq? type 'ipc)
                           ((web-rkt-handle ww-current-handle) cmd)
                           (error "ffi implementation not implemented")))
               )
          (let ((r (convert-result result)))
            (check-nok cmd r)
            r))))

  (define (ww-cmd-nok? r)
    (if (cmdr? r)
        (eq? (cmdr-ok r) #f)
        (eq? r 'cmd-nok)))

  ;;;;;;;;;;;; Check functions and converter functions

  ;;;;; Check functions
  
  (define (stylesheet-or-string? st)
    (or (stylesheet? st) (string? st)))

  (define (is-icon-file? ext)
    (lambda (v)
      (let ((r #f))
        (when (string? v)
          (let ((e (string-append "." (symbol->string ext))))
            (when (string-suffix? v e)
              (when (file-exists? v)
                (set! r #t)))))
        (unless r
          (error (format "Need an existing file of type ~a, got ~a"
                         ext v)))
        r)))

  (define (html-file-exists? f)
    (if (file-exists? f)
        #t
        (let* ((cwd (ww-cwd))
               (full-file (build-path cwd f)))
          (ww-debug (format "file-exists? '~a'" full-file))
          (file-exists? full-file)
          )
        )
    )

  (define (html-or-file? v)
    (if (file-exists? v)
        #t
        (string? v)))

  (define (symbol-or-string? s)
    (or (symbol? s) (string? s)))

  (define (any? v)
    #t)

  (define (path-or-string? s)
    (or (path? s) (string? s)))

  (define (selector? s)
    (or (symbol? s) (string? s)
        (if (list? s)
            (if (null? s)
                #f
                (letrec ((f (λ (l)
                              (if (null? l)
                                  #t
                                  (and (or (symbol? (car l))
                                           (string? (car l)))
                                       (f (cdr l))))
                              )
                            ))
                  (f s))
                )
            #f)
        )
    )

  ;;;;; Conversion functions

  (define (to-selector v)
    (if (symbol? v)
        (format "#~a, ~a" v v)
        (if (string? v)
            v
            (if (list? v)
                (letrec ((f (λ (l)
                              (if (null? l)
                                  ""
                                  (string-append ", " (to-selector (car l))
                                                 (f (cdr l)))))
                            ))
                  (string-append (to-selector (car v))
                                 (f (cdr v))))
                ""))))
  
  (define (convert-cmd-result-to type str)
    (ww-debug type)
    (ww-debug str)
    (cond
      ((or (eq? type 'number) (eq? type 'int) (eq? type 'real)) (string->number str))
      ((eq? type 'symbol) (string->symbol str))
      ((eq? type 'json) (ww-from-json str))
      ((eq? type 'stylesheet) (string->stylesheet str))
      ((eq? type 'ww-win) (make-ww-win (string->number str)))
      ((eq? type 'void) 'void)
      ((eq? type 'css-style) (string->css-style str))
      ((eq? type 'path) (string->path (substring (substring str 0 (- (string-length str) 1)) 1)))
      (else str)))

  (define (check-cmd-type v vname type typename)
    (let ((is-type (type v)))
      (unless is-type
        (error
         (format "Expected ~a of type ~a, got '~a'"
                 vname typename v)))
      #t))

  (define (convert-arg-to-cmd v vname type)
    (cond
      ((eq? type 'symbol?) v)
      ((eq? type 'string?) v)
      ((or (eq? type 'stylesheet?)
           (eq? type 'stylesheet-or-string?))
       (let* ((css (if (stylesheet? v)
                        (stylesheet->string v)
                        v))
               (h (let ((h (make-hasheq)))
                    (hash-set! h 'css css)
                    h))
               (json (jsexpr->string h))
               )
         json))
      ((eq? type 'ww-win?) (ww-win-id v))
      ((eq? type 'is-menu?) (menu->json v))
      ((eq? type 'html-file-exists?) (to-server-file v))
      ((eq? type 'html-or-file?) (if (file-exists? v)
                                     (to-server-file v)
                                     v))
      ((eq? type 'any?) (as-string v))
      ((eq? type 'selector?) (to-selector v))
      ((eq? type 'css-style?) (css-style->string v))
      ((eq? type 'boolean?) (if (eq? v #f) 'false 'true))
      ((eq? type 'symbol-or-string?) v)
      ((eq? type 'number?) v)
      ((eq? type 'path-or-string?) (if (string? v)
                                       v
                                       (path->string v)))
      (else (begin
              (ww-error (format "Convert-arg-to-cmd Unexpected: ~a ~a ~a" vname type v))
              v))))


  ;;;; Generic syntax to define commands
  (define-syntax def-cmd-check
    (syntax-rules ()
      ((_ var type)
       (check-cmd-type var 'var type 'type))
      )
    )

  (define-syntax check-opt
    (syntax-rules ()
      ((_ i (var type) args)
       (begin
         (unless (>= i (length args))
           (check-cmd-type (list-ref args i)
                           'var
                           type 'type)
           (set! i (+ i 1)))))
      )
    )

  (define-syntax def-cmd-opt-checks
    (syntax-rules ()
      ((_ (vt ...) more)
       (let ((i 0))
         (begin
           (check-opt i vt more)
           ...)
         (when (< i (length more))
           (error "Too many arguments given"))))
      )
    )

  (define (mk-cmd-arg* v vname type)
    (let ((o (open-output-string)))
      (display " " o)
      (write (convert-arg-to-cmd v vname type) o)
      (get-output-string o)))

  (define-syntax mk-cmd-arg
    (syntax-rules ()
      ((_ a type)
       (mk-cmd-arg* a 'a 'type))))

  (define-syntax mk-cmd-opt-args
    (syntax-rules ()
      ((_ c ((a t) ...) more)
       (letrec ((f (lambda (m ts)
                     (if (null? m)
                         ""
                         (string-append (mk-cmd-arg* (car m) (caar ts) (cadar ts))    ;;; TODO -- Hier convert-arg-to-cmd in plotten, want dit worden symbols en dat is niet de bedoeling. 
                                        (f (cdr m) (cdr ts)))))))
         (set! c (string-append c (f more (list '(a t) ...)))))
       )
      )
    )

  (define-syntax mk-cmd-call
    (syntax-rules ()
      ((_ cmd type output-cvt)
       (let ((r (ww-cmd cmd)))
         (ww-debug (cmdr->list r))
         (if (cmdr-ok r)
             (output-cvt (convert-cmd-result-to type (cmdr-r r)))
             'cmd-nok))
       )
      )
    )

  (define-syntax mk-func-def
    (syntax-rules ()
      ((_ func cmd () () args type output-cvt)
       (define (func)
         (let ((c (format "~a" 'cmd)))
           (mk-cmd-call c type output-cvt)))
       )
      ((_ func cmd ((a t) ...) () args type output-cvt)
       (define (func a ...)
         (begin
           (def-cmd-check a t)
           ...)
         (let ((c (format "~a" 'cmd)))
           (begin
             (set! c (string-append c (mk-cmd-arg a t)))
             ...)
           (mk-cmd-call c type output-cvt)
           ))
       )
      ((_ func cmd () ((a t) ...) args type output-cvt)
       (define (func . args)
         (def-cmd-opt-checks ((a t) ...) args)
         (let ((c (format "~a" 'cmd)))
           (mk-cmd-opt-args c ((a t) ...) args)
           (mk-cmd-call c type output-cvt)))
       )
      ((_ func cmd ((a t ) ...) ((oa ot) ...) args type output-cvt)
       (define (func a ... . args)
         (begin
           (def-cmd-check a t)
           ...)
         (def-cmd-opt-checks ((oa ot) ...) args)
         (let ((c (format "~a" 'cmd)))
           (begin
             (set! c (string-append c (mk-cmd-arg a t)))
             ...)
           (mk-cmd-opt-args c ((oa ot) ...) args)
           (mk-cmd-call c type output-cvt)))
       )
      )
    )

  (define-syntax id-converter
    (syntax-rules ()
      ((_ val) val)))

  (define-syntax def-cmd
    (syntax-rules (args)
      ((_ func cmd mandatories optionals -> type)
       (mk-func-def func cmd mandatories optionals args 'type id-converter))
      ((_ func cmd mandatories optionals -> type => output-converter)
       (mk-func-def func cmd mandatories optionals args 'type output-converter))
      )
    )

  (define-syntax def-func
    (syntax-rules ()
      ((_  func () () args body)
       (define (func)
         body))
      ((_ func ((a t) ...) () args body)
       (define (func a ...)
         (begin
           (def-cmd-check a t)
           ...)
         (begin body)))
      ((_ func () ((oa ot) ...) args body)
       (define (func . args)
         (def-cmd-opt-checks ((oa ot) ...) args)
         (begin body)))
      ((_ func ((a t) ...) ((oa ot) ...) args body)
       (define (func a ... . args)
         (begin
           (def-cmd-check a t)
           ...)
         (def-cmd-opt-checks ((oa ot) ...) args)
         (begin body))
       )
      )
    )

  (define-syntax define/typed
    (syntax-rules (args)
      ((_ (func mandatories optionals) body)
       (def-func func mandatories optionals args body)
       )
      )
    )

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web Wire Commands
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Set the log level of webui-wire
  (def-cmd ww-log-level
    loglevel () ((level symbol?)) -> symbol)

  ;; Get the spoken protocol by webui-wire
  (def-cmd ww-protocol
    protocol () () -> int)

  ;; Get/set the current directory of webui-wire
  (def-cmd ww-cwd
    cwd () [(path path-or-string?)] -> path)

  ;; Global stylesheet
  (def-cmd ww-set-stylesheet
    set-stylesheet ((st stylesheet-or-string?)) () -> void)
    
  (def-cmd ww-get-stylesheet
    get-stylesheet () () -> stylesheet)

  ;; New window
  (def-cmd ww-new
    new ((profile symbol?)) [(use-browser boolean?) (parent ww-win?)]
    -> ww-win)
  
  ;; Close window
  (def-cmd ww-close
    close ((win-id ww-win?)) [] -> void)

  ;; Move window
  (def-cmd ww-move
    move ((win-id ww-win?) (x number?) (y number?)) [] -> void)

  ;; Resize window
  (def-cmd ww-resize
    resize ((win-id ww-win?) (width? number?) (height number?)) [] -> void)

  ;; Set title of window
  (def-cmd ww-set-title
    set-title ((win-id ww-win?) (title string?)) [] -> void)

  ;; Set icon of window
  (def-cmd ww-set-icon
    set-icon ((win-id ww-win?)
              (svg-file (is-icon-file? 'svg))
              (png-file (is-icon-file? 'png))) [] -> void)
  
  ;; Set menu of window
  (def-cmd ww-set-menu
    set-menu ((win-id ww-win?)
              (menu is-menu?)) [] -> void)
  

  (define (new-handle)
    #t)

  ;; set url
  (def-cmd ww-set-url
    set-url ((win-id ww-win?)
             (url string?)) () -> void)
  
  ;; Set html of window to file 
  (def-cmd ww-set-html-file
    set-html ((win-id ww-win?)
              (html-file html-file-exists?)) ()
    -> number)

  ;; Set inner html of an Id of the HTML in the window
  (def-cmd ww-set-inner-html
    set-inner-html ((win-id ww-win?)
                    (element-id symbol-or-string?)
                    (html-of-file html-or-file?)) () -> void)
  

  ;; Het the inner html of an id of the HTML in the window
  (def-cmd ww-get-inner-html
    get-inner-html ((win-id ww-win?)
                    (element-id symbol-or-string?)) () -> json)

  ;; Set attribute of element in html
  (def-cmd ww-set-attr
    set-attr ((win-id ww-win?)
              (element-id symbol-or-string?)
              (attr symbol-or-string?)
              (val any?)) () -> void)

  ;; Get attribute value of element in html
  (def-cmd ww-get-attr
    get-attr ((win-id ww-win?)
              (element-id symbol-or-string?)
              (attr symbol-or-string?)) () -> json)
  
  ;; Get all attributes of an element with given id

  (define (mk-attrs r)
    (let ((attrs (make-hash)))
      (for-each (λ (attr-val)
                  (hash-set! attrs
                             (string->symbol (car attr-val))
                             (cadr attr-val)))
                r)
      attrs))
  
  (def-cmd ww-get-attrs
    get-attrs ((win-id ww-win?)
               (element-id symbol-or-string?)) () -> json
    -> mk-attrs)

  ;(define (ww-get-attrs w id)
  ;  (let ((r (ww-get-attrs* w id)))
  ;    (if (ww-cmd-nok? r)
  ;        r
  ;        (mk-attrs r))))

  ;; Get info of all elements for a selector
  (def-cmd ww-get-elements
    get-elements ((win-id ww-win?)
                  (selector selector?)) () -> json
    -> (λ (r)
         (map (λ (item)
             (cons (string->symbol (car item))
                   (mk-attrs (cadr item))))
           r)))


  ;(define (ww-get-elements win-id selector)
  ;  (let ((r (ww-get-elements* win-id selector)))
  ;    (map (λ (item)
  ;           (cons (string->symbol (car item))
  ;                 (mk-attrs (cadr item))))
  ;         r)))

  ;; Delete attribute of element
  (def-cmd ww-del-attr
    del-attr ((win-id ww-win?)
              (element-id symbol-or-string?)
              (attr symbol-or-string?)
              ) () -> void)

  (define (ww-await . args)
    #t)
  
  ;; get value of an element
  (def-cmd ww-get-value
    value ((win-id ww-win?)
           (element-id symbol-or-string?)) () -> json)

  ;; set value of an element
  (def-cmd ww-set-value
    value ((win-id ww-win?)
           (element-id symbol-or-string?)
           (value any?)) () -> void)
    
  ;; Bind some CSS selector to an event, given that each
  ;; element that satisfies the selector has to have an id.
  ;; Note: get-elements, also working on selectors, will
  ;; assign an id to all elements that satisfy the selector
  ;; without one. 
  ;; Returns list of lists of id, tag an type attribute of
  ;; each element that has been bound. 
  (def-cmd ww-bind
    bind ((win-id ww-win?)
          (event symbol-or-string?)
          (selector selector?)) () -> json
    -> (λ (r)
         (map (λ (item)
                (map string->symbol item))
              r)))

  ;; Bind an element with the given id to the event
  (def-cmd ww-on
    on ((win-id ww-win?)
        (event symbol-or-string?)
        (id symbol-or-string?)) () -> void)

  ;; Element info
  (def-cmd ww-element-info
    element-info ((win-id ww-win?)
                  (element-id symbol-or-string?)) ()
    -> json
    -> (λ (r)
         (list (string->symbol (car r))
               (if (string=? (cadr r) "")
                   #f
                   (string->symbol (cadr r)))
               (if (string=? (caddr r) "")
                   #f
                   (string->symbol (caddr r)))
               (cadddr r)))
    )
               

  ;; Add a class to an element
  (def-cmd ww-add-class
    add-class ((win-id ww-win?)
               (element-id symbol-or-string?)
               (class symbol-or-string?))
    () -> void)
  
  ;; Remove a class from an element
  (def-cmd ww-remove-class
    remove-class ((win-id ww-win?)
                  (element-id symbol-or-string?)
                  (class symbol-or-string?))
    () -> void)
  
  ;; Has a class
  (define re-class-split #px"\\s+")

  (define/typed (ww-has-class? ((win-id ww-win?)
                                (element-id symbol-or-string?)
                                (class* symbol-or-string?))
                               ())
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
  (def-cmd ww-add-style
    add-style ((win-id ww-win?)
               (element-id symbol-or-string?)
               (css-style css-style?)) () -> void)
    
  ;; Set a style of an element
  (def-cmd ww-set-style
    set-style ((win-id ww-win?)
               (element-id symbol-or-string?)
               (css-style css-style?)) () -> void)
    
  ;; Get the style of an element
  (def-cmd ww-get-style
    get-style ((win-id ww-win?)
               (element-id symbol-or-string?)) ()
    -> css-style)

  ;; Show State
  (def-cmd ww-set-show-state
    set-show-state ((win-id ww-win?)
                    (state symbol?)) ()
    -> void)

  (def-cmd ww-get-show-state
    show-state ((win-id ww-win?)) ()
    -> symbol)
    
  ;; Files and directories
  ;(define (ww-file-open win-id title dir file-filters)
  ;  (let ((cmd (format "file-open ~a ~a ~a ~a" win-id
  ;                     (as-string title)
  ;                     (as-string dir)
  ;                     (as-string file-filters))))
  ;    (ww-cmd cmd)))

  (def-cmd ww-file-open
    file-open ((win-id ww-win?)
               (caption string?)
               (dir string?)
               (file-filters string?)) ()
    -> string)

  (def-cmd ww-file-save
    file-save ((win-id ww-win?)
               (caption string?)
               (dir string?)
               (file-filters string?)
               (overwrite boolean?)
               ) ()
    -> string)
    
  ;(define (ww-file-save win-id title dir file-filters overwrite)
  ;  (let ((cmd (format "file-save ~a ~a ~a ~a ~a" win-id
  ;                     (as-string title)
  ;                     (as-string dir)
  ;                     (as-string file-filters)
  ;                     (if overwrite 1 0))))
  ;    (ww-cmd cmd)))

  (def-cmd ww-choose-dir
    choose-dir ((win-id ww-win?)
                (caption string?)
                (dir string?)
                ) ()
    -> string)

  ;(define (ww-choose-dir win-id title dir)
  ;  (let ((cmd (format "choose-dir ~a ~a ~a" win-id
  ;                     (as-string title)
  ;                     (as-string dir))))
  ;    (ww-cmd cmd)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Finalizing stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;(define will (make-will-executor))

  ;(define (ww-register-finalizer obj proc)
  ;  (will-register will obj proc))

  ;(void (thread (λ () (let loop () (will-execute will) (loop)))))
  
  ); end of module