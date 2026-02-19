(module web-racket racket/gui

  (require racket/gui
           "web-wire.rkt"
           "css.rkt"
           "menu.rkt"
           "../utils/sprintf.rkt"
           "webui-wire-download.rkt"
           html-printer
           (prefix-in g: gregor)
           (prefix-in g: gregor/time)
           gregor-utils
           net/sendurl
           racket/path
           xml
           )
  
  (provide ww-element%

           ww-input%
           ww-input-email%
           ww-input-date%
           ww-input-time%
           ww-input-datetime%
           ww-input-range%
           
           ww-webview%
           ww-webview-dialog%
           ww-settings%
           ww-webview-message%

           ww-start
           ww-stop
           ww-set-debug
           ww-debug
           ww-error

           (all-from-out "css.rkt")
           (all-from-out "menu.rkt")

           ww-set-custom-webui-wire-command!
           ww-get-webui-wire-version
           ww-display-log
           ww-tail-log
           )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Regexes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define re-resize #px"([0-9]+)\\s+([0-9]+)")
  (define re-move re-resize)
  (define re-file-open #px"([0-9]+)[:]([^:]+)[:](.*)")
  (define re-choose-dir re-file-open)
  (define re-navigate #px"(.*)[:]([^:]+)[:]([^:]+)$")
  
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Class representing an element in the HTML page
  ;;   each element is identified by an id. 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ww-element%
    (class object%
      (init-field [win-id #f] [id #f])
      
      (define/public (get-win-id)
        win-id)

      (define/public (get-id)
        id)

      (define/public (win)
        (let ((w (hash-ref windows (ww-win-id win-id) #f)))
          w))

      (define connected-callbacks (make-hash))

      (define/public (callback evt . args)
        (ww-debug (format "WW-ELEMENT%: Callback for ~a - ~a - ~a" id evt args))
        (let ((cb (hash-ref connected-callbacks evt #f)))
          (unless (eq? cb #f)
            (with-handlers ([exn:fail?
                             (λ (e)
                               (ww-error (format "callback for ~a: ~a" evt e)))])
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

      (define/public (get-attr a)
        (ww-get-attr win-id id a))

      (define/public (set-attr! a val)
        (ww-set-attr win-id id a val))

      (define/public (del-attr a)
        (ww-del-attr win-id id a))

      (define/public (get-attrs)
        (ww-get-attrs win-id id))

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

      (define/public (set-inner-html! html-or-sexpr)
        (if (string? html-or-sexpr)
            (ww-set-inner-html win-id id html-or-sexpr)
            (set-inner-html! (xexpr->html5 html-or-sexpr))))

      (super-new)
      )
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Classes representing different kinds of input/textarea elements in html
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax inp-set!
    (syntax-rules ()
      ((_ var val)
       (set! var val))))
  

  ;;;; Generic input
  (define ww-input%
    (class ww-element%

      (define cb #f)
      (define val #f)

      (define/public (value-converter v)
        v)

      (define/public (get)
        val)

      (define/public (on-change! callback)
        (inp-set! cb callback)
        (cb val)
        )

      (define/public (set! v)
        (inp-set! val v)
        (ww-set-value (send this get-win-id)
                      (send this get-id) v))

      (define/override (disable)
        (super disable)
        (ww-set-attr (send this get-win-id)
                     (send this get-id) 'disabled ""))

      (define/override (enable)
        (super enable)
        (ww-del-attr (send this get-win-id)
                     (send this get-id) 'disabled))
          
      (super-new)

      (begin

        (let ((v (ww-get-value (send this get-win-id) (send this get-id))))
          (inp-set! val (send this value-converter v)))
        
        (send this connect 'input (λ (data)
                                    (ww-debug "WW-INPUT% 'input event:" data)
                                    (let ((js-evt (hash-ref data 'js_evt #f)))
                                      (ww-debug "WW-INPUT% 'input event, js-evt = " js-evt)
                                      (unless (eq? js-evt #f)
                                        (when (hash-has-key? js-evt 'value)
                                          (let ((v (hash-ref js-evt 'value)))
                                            (inp-set! val v)
                                            (unless (eq? cb #f)
                                              (cb v))
                                            ))))
                                    ))
        (send (send this win) bind 'input (format "#~a" (send this get-id)))
        )
      ))


  ;;;; Email input
  (define ww-input-email%
    (class ww-input%

      (super-new)))

  ;;;; Date input
  (define ww-input-date%
    (class ww-input%

      (define/override (get)
        (let ((val (super get)))
          (g:parse-date val "yyyy-MM-dd")))

      (define/override (set! d)
        (when (racket-date? d)
          (set! (date->moment d)))
        (unless (or (g:date? d) (g:moment? d) (g:datetime? d))
          (error "set! - gregor date expected"))
        (super set! (sprintf "%04d-%02d-%02d" (g:->year d) (g:->month d) (g:->day d)))
        d)

      (super-new)
      ))

  ;;;; Time input
  (define ww-input-time%
    (class ww-input%

      (define/override (get)
        (let ((val (super get)))
          (with-handlers ([exn:fail?
                           (λ (e) (g:parse-time val "HH:mm"))])
            (g:parse-time val "HH:mm:ss"))))

      (define/override (set! t)
        (when (racket-date? t)
          (set! (date->moment t)))
        (unless (or (g:time? t) (g:datetime? t) (g:moment? t))
          (error "set! - gregor time?, moment? or datetime? expected"))
        (super set! (sprintf "%02d:%02d:%02d" (g:->hours t) (g:->minutes t) (g:->seconds t))))

      (super-new)
      ))

  ;;;;; Date-time local
  (define ww-input-datetime%
    (class ww-input%

      (define/override (get)
        (let ((val (super get)))
          (with-handlers ([exn:fail?
                           (λ (e) (g:parse-moment val "yyyy-MM-dd'T'HH:mm:ss"))])
            (g:parse-moment val "yyyy-MM-dd'T'HH:mm"))))

      (define/override (set! m)
        (when (racket-date? m)
          (set! date->moment m))
        (unless (or (g:datetime? m) (g:moment? m) (g:date? m) (g:time? m))
          (error "set! - gregor time? , date?, datetime? or moment? expected"))
        #t)

      (super-new)
      )
    )

  ;;;; Range
  (define ww-input-range%
    (class ww-input%

      (define/override (value-converter v)
        (string->number v))
      
      (super-new)
      ))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Classes representing WebView Windows. 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define ww-webview%
    (class object%
      
      (init-field [profile 'default-profile]
                  [settings #f]
                  [use-browser #f]
                  [parent-id #f]
                  [parent #f]
                  [title "Racket HTML Window"]
                  [x (if (eq? settings #f) _std_x (send settings get 'window-x _std_x))]
                  [y (if (eq? settings #f) _std_y (send settings get 'window-y _std_y))]
                  [width (if (eq? settings #f) _std_w (send settings get 'window-width _std_w))]
                  [height (if (eq? settings #f) _std_h (send settings get 'window-height _std_h))]
                  [icon #f]
                  [menu #f]
                  [html-file #f]
                  )

      (define win-id #f)

      (define menu-cbs (make-hash))
      (define elements (make-hash))
      (define html-handle #f)

      (define/public (clone-settings section)
        (if (eq? settings #f)
            #f
            (send settings clone section)))
      
      (define (event-handler evt content)
        (ww-debug (format "win-id=~a '~a ~a" win-id evt content))
        (cond
          ([eq? evt 'page-loaded] (let ((page-handle (hash-ref content 'page_handle 'none)))
                                    (ww-debug (format "html-handle: ~a, page-handle: ~a, equal?: ~a"
                                                      html-handle
                                                      page-handle
                                                      (equal? html-handle page-handle)))
                                    (when (and (number? html-handle) (number? page-handle) (= html-handle page-handle))
                                      (send this html-loaded))))
          ([eq? evt 'click] (handle-click (string->symbol (hash-ref content 'id)) content))
          ([eq? evt 'dblclick] (handle-dblclick (string->symbol (hash-ref content 'id)) content))
          ([eq? evt 'contextmenu] (handle-contextmenu (string->symbol (hash-ref content 'id)) content))
          ([eq? evt 'input] (handle-input (string->symbol (hash-ref content 'id)) content))
          ([eq? evt 'change] (handle-change (string->symbol (hash-ref content 'id)) content))
          ([eq? evt 'resized] (let* ((width* (hash-ref content 'width))
                                     (height* (hash-ref content 'height))
                                     )
                                (set! width width*)
                                (set! height height*)
                                (unless (eq? settings #f)
                                  (send settings set 'window-width width)
                                  (send settings set 'window-height height))
                                ))
          ([eq? evt 'moved] (let* ((x* (hash-ref content 'x))
                                   (y* (hash-ref content 'y))
                                   )
                              (set! x x*)
                              (set! y y*)
                              (unless (eq? settings #f)
                                (send settings set 'window-x x)
                                (send settings set 'window-y y))
                              ))
          ([eq? evt 'close-request] (when (send this can-close?)
                                      (send this close)))
          ([eq? evt 'menu-item-choosen] (let* ((menu-id (string->symbol (hash-ref content 'item)))
                                               (cb (hash-ref menu-cbs menu-id #f)))
                                          (unless (eq? cb #f)
                                            (cb))))
          ([eq? evt 'navigate] (let* ((url (hash-ref content 'url))
                                      (kind (string->symbol (hash-ref content
                                                                      'navigation-kind)))
                                      (type (string->symbol (hash-ref content
                                                                      'navigation-type)))
                                      )
                                (send this handle-navigate url type kind)))
          ([eq? evt 'choose-dir] (let* ((handle (hash-ref content 'handle))
                                        (choosen (hash-ref content 'choosen))
                                        (dir (hash-ref content 'dir))
                                        )
                                   (send this dir-choosen handle choosen dir)))
          ([eq? evt 'inner-html-set] (let* ((handle (hash-ref content 'handle))
                                            (has-been-set (hash-ref content 'oke)))
                                       (send this inner-html-set handle has-been-set)))
          )
        )

      (define/public (handle-click element-id data)
        (let ((el (hash-ref elements element-id #f)))
          (unless (eq? el #f)
            (send el callback 'click data))))

      (define/public (handle-dblclick element-id data)
        (let ((el (hash-ref elements element-id #f)))
          (unless (eq? el #f)
            (send el callback 'dblclick data))))

      (define/public (handle-contextmenu element-id data)
        (let ((el (hash-ref elements element-id #f)))
          (unless (eq? el #f)
            (send el callback 'contextmenu data))))

      (define/public (handle-change element-id data)
        (let ((el (hash-ref elements element-id #f)))
          (unless (eq? el #f)
            (send el callback 'change data 'value))))

      (define/public (handle-input element-id data)
        (let ((el (hash-ref elements element-id #f)))
          (unless (eq? el #f)
            (send el callback 'input data))))

      (define/public (handle-navigate url type kind)
        (let ((method (if (eq? kind 'set-html) 'set-html-file! 'set-url)))
          (cond
            ([eq? type 'standard]
             (dynamic-send this method url))
            (else (ww-error (format "Don't know what to do for ~a - ~a" type url)))
            )
          )
        )

      (define/public (get-win-id) win-id)

      (define (cl-selector tag type)
        (cond
          ([eq? tag 'INPUT]
           (cond
             ([eq? type 'text] ww-input%)
             ([eq? type 'date] ww-input-date%)
             ([eq? type 'datetime-local] ww-input-datetime%)
             ([eq? type 'range] ww-input-range%)
             (else ww-input%)))
          (else ww-element%)))

      (define/public (bind event selector . forced-cl)
        (ww-debug (format "call to bind ~a ~a ~a" event selector forced-cl))
        (let ((infos (ww-bind win-id event selector)))
          (for-each (λ (info)
                      (let* ((id (car info))
                             (tag (cadr info))
                             (type (caddr info)))
                        (ww-debug (format "bind: ~a ~a ~a" id tag type))
                        (let ((cl (if (null? forced-cl)
                                      (cl-selector tag type)
                                      (car forced-cl))))
                          (unless (hash-has-key? elements id)
                            (hash-set! elements id 'in-the-making)
                            (hash-set! elements id
                                       (new cl [win-id win-id] [id id]))))))
                    infos)))

      (define/public (bind-inputs)
        (bind 'change 'input )
        (bind 'change 'textarea)
        #t
        )

      (define/public (bind-buttons)
        (bind 'click 'button)
        )

      (define/public (element id)
        (unless (hash-has-key? elements id)
          (let ((info (ww-element-info win-id id)))
            (let* ((el-id (car info))
                   (tag (cadr info))
                   (type (caddr info))
                   (exist (cadddr info))
                   )
              (unless exist
                (ww-debug (format "Element ~a does not exist!" id)))
              (let* ((cl (cl-selector tag type))
                     (obj (new cl [win-id win-id] [id id])))
                (hash-set! elements el-id obj)
                ))
            (element id))
          )
        (hash-ref elements id))

      (define/public (new-element id . cl-selector)
        (unless (hash-has-key? elements id)
          (let ((cl (if (null? cl-selector) ww-element% (car cl-selector))))
            (let ((obj (new cl [win-id win-id] [id id])))
              (hash-set! elements id obj))))
        (hash-ref elements id))

      (define/public (get-elements selector)
        (ww-get-elements win-id selector))

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

      (define/public (set-icon! icn)
        (ww-set-icon win-id icn))

      (define/public (set-html-file! file)
        (let* ((full-path (if (string? file)
                              (string->path file)
                              file))
               (folder (path-only full-path))
               (the-file (file-name-from-path full-path))
               )
          (ww-debug (format "set-html-file! ~a, ~a" folder the-file))
          (set! html-file file)
          (when folder 
            (ww-cwd folder))
          (set! html-handle (ww-set-html-file win-id the-file))
          (ww-debug (format "html file set to ~a" the-file))
          ))

      (define/public (set-html html)
        (let* ((tmpfile "/tmp/test.html")
               (fh (open-output-file tmpfile #:exists 'replace)))
          (displayln tmpfile)
          (display html fh)
          (close-output-port fh)
          (send this set-html-file! tmpfile)
          )
        )

      (define/public (set-url url)
        (send-url url))

      (define/public (html-loaded)
        (send this bind-buttons)
        (send this bind-inputs))

      (define/public (get-html-file)
        html-file)

      (define/public (show)
        (ww-set-show-state win-id 'show))

      (define/public (hide)
        (ww-set-show-state win-id 'hide))

      (define/public (maximize)
        (ww-set-show-state win-id 'maximize))

      (define/public (normalize)
        (ww-set-show-state win-id 'normalize))

      (define/public (minimize)
        (ww-set-show-state win-id 'minimize))

      (define/public (fullscreen)
        (ww-set-show-state win-id 'fullscreen))

      (define/public (show-state)
        (ww-get-show-state win-id))

      (define/public (can-close?)
        #t)
      
      (define/public (close)
        (ww-close win-id)
        (hash-remove! windows (ww-win-id win-id))
        (hash-remove! windows-evt-handlers (ww-win-id win-id))
        (when (= (hash-count windows) 0)
          (ww-stop))
        )

      (define/public (set-menu! menu-def)
        (ww-set-menu win-id menu-def))

      (define/public (connect-menu! id cb)
        (hash-set! menu-cbs id cb))

      (define/public (disconnect-menu! id)
        (hash-remove! menu-cbs id)
        )

      (define/public (popup-menu menu-def x y)
        (ww-popup-menu win-id menu-def x y)
        (let* ((ids (list))
               (clear-connections (λ ()
                                   (for-each (λ (id)
                                               (send this disconnect-menu! id))
                                             ids))))
          (menu-for-each menu-def
                         (λ (item)
                           (let ((cb (ww-menu-item-callback item))
                                 (id (ww-menu-item-id item)))
                             (set! ids (cons id ids))
                             (send this connect-menu! id (λ ()
                                                           (clear-connections)
                                                           (cb))))))
          (connect-menu! (ww-menu-id menu-def) (λ () (clear-connections)))
          (set! ids (cons (ww-menu-id menu-def) ids))
          )
        )

      ; files and directories
      (define/public (file-open caption base-dir filters)
        (let ((r (ww-file-open win-id caption base-dir filters)))
          (if (eq? (car r) #f)
              #f
              (let ((m (regexp-match re-file-open (cdr r))))
                (if (eq? m #f)
                    #f
                    (let ((file (cadddr m)))
                      (ww-from-string file))
                    )
                )
              )
          )
        )

      (define/public (file-save caption base-dir filters . overwrite)
        (let ((o (if (null? overwrite) #f (car overwrite))))
          (let ((r (ww-file-save win-id caption base-dir filters o)))
            (if (eq? (car r) #f)
                #f
                (let ((m (regexp-match re-file-open (cdr r))))
                  (if (eq? m #f)
                      #f
                      (let ((file (cadddr m)))
                        (ww-from-string file))
                      )
                  )
                )
            )
          )
        )

      (define/public (choose-dir caption base-dir)
        (let ((r (ww-choose-dir win-id caption base-dir)))
          (ww-debug (format "choose-dir: ~a" r))
          (if (eq? r 'cmd-nok)
              #f
              r)))

      (define/public (dir-choosen handle choosen dir)
        (ww-debug (format "dir-choosen: handle=~a, choosen=~a, dir=~a" handle choosen dir))
        )

      (define/public (inner-html-set handle oke)
        (ww-debug (format "inner-html-set ~a: ~a" handle oke))
        )

      (define/public (inherit-checks)
        #t)

      ; Supers first
      (super-new)

      ; construct
      (begin
        (send this inherit-checks)
        
        (when (= (hash-count windows) 0)
          (ww-start))

        (when (eq? parent-id #f)
          (unless (eq? parent #f)
            (set! parent-id (send parent get-win-id))))

        (when (eq? parent #f)
          (unless (eq? parent-id #f)
            (set! parent (ww-get-window-for-id parent-id))))
        
        (next-window-init-position)
        
        (set! win-id
              (if (eq? parent-id #f)
                  (ww-new profile use-browser)
                  (ww-new profile use-browser parent-id)))
        (when (eq? win-id #f)
          (error "Window could not be constructed"))

        (hash-set! windows-evt-handlers (ww-win-id win-id) event-handler)
        (hash-set! windows (ww-win-id win-id) this)

        (ww-resize win-id width height)

        (when parent
            (let* ((parent-width (send parent get-width))
                   (parent-height (send parent get-height))
                   (parent-x (send parent get-x))
                   (parent-y (send parent get-y))
                   )
              (set! x (+ parent-x (/ (- parent-width width) 2)))
              (set! y (+ parent-y (/ (- parent-height height) 2)))
              )
          )
        (ww-move win-id x y)
        
        (send this set-title! title)
        
        (unless (eq? icon #f)
          (send this set-icon! icon))
        
        (unless (eq? menu #f)
          (send this set-menu! menu))
        
        (unless (eq? html-file #f)
          (send this set-html-file! html-file))
        )

    ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Classes representing WebView Dialogs
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ww-webview-dialog%
    (class ww-webview%
      (super-new)

      (inherit-field parent)

      (define/override (inherit-checks)
        (when (eq? parent #f)
          (error "A parent must be given")))
      ))


  (define ww-webview-message%
    (class ww-webview-dialog%
      (super-new)

      (send this set-html
            "<html><head><title>Message</title></head><body><h3 id=\"msg\">[msg]</h3><p id=\"submsg\">[submsg]</p></body></html>"
            )
      )
    )

  (define (set-global-stylesheet st)
    (ww-set-stylesheet st))

  (define (get-global-stylesheet)
    (ww-get-stylesheet))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Classes for settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ww-settings%
    (class object%
      (super-new)
      (define/public (set key value)
        (error "ww-settings%: set not implemented, override in your specific subclass"))
      (define/public (get key . default)
        (error "ww-settings%: get not implemented, override in your specific subclass"))
      (define/public (clone new-section)
        (error "ww-settings%: clone not implemented, override in your specific subclass"))
      (define/public (set! key value)
        (send this set key value))
      )
    )
 
  ); end of module