(module web-racket racket/gui

  (require racket/gui
           "web-wire.rkt"
           "css.rkt"
           html-printer
           )
  
  (provide ww-element%
           ww-input%
           ww-window%

           ww-start
           ww-stop
           ww-set-debug
           ww-debug
           ww-error

           (all-from-out "css.rkt")
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

      (define/override (disable)
        (super disable)
        (ww-set-attr (send this get-win-id) (send this get-id) 'disabled ""))

      (define/override (enable)
        (super enable)
        (ww-del-attr (send this get-win-id) (send this get-id) 'disabled))
          
      (super-new)))

  (define ww-input-date%
    (class ww-input%

      (define/override (get)
        (let ((val (super get)))
          val))

      (super-new)
      ))

  (define ww-window%
    (class object%
      
      (init-field [profile 'default-profile]
                  [parent-id #f]
                  [parent #f]
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

      (define menu-cbs (make-hash))
      (define elements (make-hash))
      
      (define (event-handler type evt content)
        (ww-debug (format "win-id=~a '~a '~a ~a" win-id type evt content))
        (cond
          ([eq? evt 'page-loaded] (send this html-loaded))
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
          ([eq? evt 'request-close] (when (send this can-close?)
                                      (send this close)))
          ([eq? evt 'menu-item-choosen] (let* ((menu-id (string->symbol content))
                                               (cb (hash-ref menu-cbs menu-id #f)))
                                          (unless (eq? cb #f)
                                            (cb))))
          ([eq? evt 'navigate] (let* ((m (regexp-match re-navigate content))
                                      (url (ww-from-string (cadr m)))
                                      (type (string->symbol (caddr m)))
                                      (kind (string->symbol (cadddr m)))
                                      )
                                (send this handle-navigate url type kind)))
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

      (define/public (handle-navigate url type kind)
        (let ((method (if (eq? kind 'set-html) 'set-html-file! 'set-url)))
          (cond
            ([eq? type 'link-clicked]
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
             (else ww-input%)))
          (else ww-element%)))

      (define/public (bind event selector . forced-cl)
        (let ((infos (ww-bind win-id event selector)))
          (for-each (λ (info)
                      (let* ((id (car info))
                             (tag (cadr info))
                             (type (caddr info)))
                        (ww-debug (format "bind: ~a ~a ~a" id tag type))
                        (let ((cl (if (null? forced-cl)
                                      (cl-selector tag type)
                                      (car forced-cl))))
                          (hash-set! elements id
                                     (new cl [win-id win-id] [id id])))))
                    infos)))

      (define/public (bind-inputs)
        (bind 'change 'input )
        (bind 'change 'textarea)
        )

      (define/public (bind-buttons)
        (bind 'click 'button)
        )

      (define/public (element id)
        (let ((el (hash-ref elements id 'no-element-with-id-in-hash)))
          (if (eq? el 'no-element-with-id-in-hash)
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
              el)))

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

      (define/public (set-html-file! file)
        (set! html-file file)
        (ww-set-html win-id html-file))

      (define/public (set-url url)
        (ww-set-url win-id url))

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
        (ww-show-state win-id))

      (define/public (can-close?)
        #t)
      
      (define/public (close)
        (ww-close win-id)
        (hash-remove! windows win-id)
        (hash-remove! windows-evt-handlers win-id)
        (when (= (hash-count windows) 0)
          (ww-stop))
        )

      (define/public (set-menu menu-def)
        (ww-set-menu win-id menu-def))

      (define/public (connect-menu id cb)
        (hash-set! menu-cbs id cb))

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
          (if (eq? (car r) #f)
              #f
              (let ((m (regexp-match re-choose-dir (cdr r))))
                (if (eq? m #f)
                    #f
                    (let ((dir (caddr m)))
                      (ww-from-string dir))
                    )
                )
              )
          )
        )
               
      ; construct
      (begin
        (when (= (hash-count windows) 0)
          (ww-start))

        (when (eq? parent-id #f)
          (unless (eq? parent #f)
            (set! parent-id (send parent get-win-id))))

        (when (eq? parent #f)
          (unless (eq? parent-id #f)
            (set! parent (ww-get-window-for-id parent-id))))
        
        (next-window-init-position)
        
        (set! win-id (ww-new profile parent-id))
        (when (eq? win-id #f)
          (error "Window could not be constructed"))

        (hash-set! windows-evt-handlers win-id event-handler)
        (hash-set! windows win-id this)

        (ww-move win-id x y)
        (ww-resize win-id width height)
        
        (ww-set-title win-id title)
        
        (unless (eq? icon #f)
          (ww-set-icon win-id icon))
        
        (unless (eq? menu #f)
          (ww-set-menu win-id menu))
        
        (unless (eq? html-file #f)
          (ww-set-html win-id html-file))
        )

      (super-new)
    ))

  (define (set-global-stylesheet st)
    (ww-set-stylesheet st))

  (define (get-global-stylesheet)
    (ww-get-stylesheet))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Testing stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define test-menu
    '(("File" (("Open" open) ("Close" close) ("Quit" quit)))
      ("Edit" (("Copy" copy) ("Advanced" (("Copy 1" copy1) ("Copy 2" copy2)))
                              ("Cut" cut) ("Paste" paste)))
      ))

  (define test-dialog%
    (class ww-window%
      (super-new [html-file "../../web-wire/test/dialog.html"]
                 [width 400]
                 [height 300])

      (define/override (html-loaded)
        (super html-loaded)
        (let* ((btn (send this element 'ok-btn)))
          (send btn connect 'click (λ (data)
                                     (send this close)))))
      ))

  (define test-window%
    (class ww-window%
      (super-new [html-file "../../web-wire/test/test1.html"])

      (define/override (html-loaded)
        (super html-loaded)
        (let* ((btn (send this element 'app-button)))
          (send btn connect  'click (λ (data)
                                (new test-dialog% [parent this]))))
          )

      (begin
        (send this set-menu test-menu)
        (send this connect-menu 'quit (λ () (send this close)))
        )
      )
    )
    
  ); end of module