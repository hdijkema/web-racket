(module menu racket/base

  (require json)
  
  (provide menu
           menu-item
           is-menu?
           menu-set-callback!
           menu-set-icon!
           menu-set-title!
           menu->json
           with-menu-item
           menu-for-each
           ww-menu-item-callback
           )


  (define-struct ww-menu-item
    (id [title #:mutable] [icon-file #:mutable] [callback #:mutable] [submenu #:mutable] [separator #:mutable])
    #:transparent)
  
  (define-struct ww-menu
    ([items #:mutable])
    #:transparent
    )

  (define (is-menu? mnu)
    (if (ww-menu? mnu)
        (if (list? (ww-menu-items mnu))
            (letrec ((f (lambda (m)
                          (if (null? m)
                              #t
                              (if (ww-menu-item? (car m))
                                  (if (eq? (ww-menu-item-submenu (car m)) #f)
                                      (f (cdr m))
                                      (and (is-menu? (ww-menu-item-submenu (car m)))
                                           (f (cdr m))))
                                  #f)
                              ))
                        ))
              (f (ww-menu-items mnu)))
            #f)
        #f))

  (define (menu . items)
    (make-ww-menu items))

  (define (menu-item id title
                     #:icon-file [icon-file #f]
                     #:callback [callback (lambda args #t)]
                     #:submenu [submenu #f]
                     #:separator [separator #f])
    (unless (symbol? id)
      (error "menu-item needs an id of symbol?"))
    (unless (string? title)
      (error "menu-item needs a title of string?"))
    (unless (or (eq? icon-file #f) (string? icon-file) (path? icon-file))
      (error "menu-item's optional argument icon-file must be #f,  string? or path?"))
    (unless (or (eq? submenu #f) (is-menu? submenu))
      (error "menu-item's optional argument submenu must be #f or is-menu?"))
    (unless (boolean? separator)
      (error "menu-item's optional argument separator must be boolean?"))
    (make-ww-menu-item id title icon-file callback submenu separator))

  (define (menu->hash menu)
    (unless (is-menu? menu)
      (error "menu->hash must be called with a menu"))
    (let* ((items (ww-menu-items menu))
           (r (map (λ (item)
                     (let ((h (make-hasheq)))
                       (hash-set! h 'id (format "~a" (ww-menu-item-id item)))
                       (hash-set! h 'name (ww-menu-item-title item))
                       (unless (eq? (ww-menu-item-icon-file item) #f)
                         (hash-set! h 'icon (ww-menu-item-icon-file item)))
                       (unless (eq? (ww-menu-item-submenu item) #f)
                         (hash-set! h 'submenu (menu->hash (ww-menu-item-submenu item))))
                       (unless (eq? (ww-menu-item-separator item) #f)
                         (hash-set! h 'separator #t))
                       h
                       )) items))
           )
      (let ((h (make-hasheq)))
        (hash-set! h 'menu r)
        h)))

  (define (menu-for-each menu cb)
    (let ((items (ww-menu-items menu)))
      (letrec ((f (λ (items)
                    (if (null? items)
                        #t
                        (let ((item (car items)))
                          (let ((submenu (ww-menu-item-submenu item)))
                            (if (eq? submenu #f)
                                (cb item)
                                (menu-for-each submenu cb))))
                        )
                    )
                  ))
        (f items))))

  (define (menu->json menu)
    (let ((o (open-output-string)))
      (write-json (menu->hash menu) o)
      (get-output-string o)))

  (define (find-menu-item menu id)
    (let ((items (ww-menu-items menu)))
      (letrec ((f (λ (items)
                    (if (null? items)
                        #f
                        (let ((item (car items)))
                          (if (eq? (ww-menu-item-id item) id)
                              item
                              (let ((submenu (ww-menu-item-submenu item)))
                                (if (eq? submenu #f)
                                    (f (cdr items))
                                    (let ((found-item (find-menu-item submenu id)))
                                      (if (eq? found-item #f)
                                          (f (cdr items))
                                          found-item))
                                    ))
                              ))
                        ))
                  ))
        (f items))))

  (define (with-menu-item menu id cb)
    (unless (is-menu? menu)
      (error "menu must be of is-menu?"))
    (unless (symbol? id)
      (error "id must be of symbol?"))
    (let ((item (find-menu-item menu id)))
      (if (eq? item #f)
          (error (format "cannot find id'~a in given menu" id))
          (cb item)))
    menu)
      
  (define (menu-set-title! menu id title)
    (unless (string? title)
      (error "title must be of string?"))
    (with-menu-item menu id
      (λ (item)
        (set-ww-menu-item-title! item title))))
  
  (define (menu-set-icon! menu id icon)
    (unless (or (eq? icon #f) (path? icon) (string? icon))
      (error "title must be of #f, string? or path?"))
    (with-menu-item menu id
      (λ (item)
        (set-ww-menu-item-icon-file! item icon))))

  (define (menu-set-callback! menu id cb)
    (unless (procedure? cb)
      (error "callback must be of procedure?"))
    (with-menu-item menu id
      (λ (item)
        (set-ww-menu-item-callback! item cb))))
          
  ); end of module





  
  