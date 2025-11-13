#lang racket/gui

(require "../main.rkt"
         racket/runtime-path
         racket/gui
         simple-ini/class
         )

(provide
 (all-from-out racket/gui)
 example-1-window%
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path html-start "example-1.html")
(define-runtime-path dialog-html "example-1-dialog.html")

(define test-menu (menu (menu-item 'm-file "File"
                                   #:submenu
                                   (menu (menu-item 'm-open "Open File")
                                         (menu-item 'm-close "Close File")
                                         (menu-item 'm-select-dir "Select Folder" #:separator #t)
                                         (menu-item 'm-quit "Quit" #:separator #t)))
                        (menu-item 'm-edit "Edit"
                                   #:submenu
                                   (menu (menu-item 'm-copy "Copy")
                                         (menu-item 'm-cut  "Cut")
                                         (menu-item 'm-paste "Paste")
                                         (menu-item 'm-prefs "Preferences" #:separator #t)
                                         ))
                        (menu-item 'm-auto "Processes"
                                   #:submenu
                                   (menu (menu-item 'm-start "Start counter")
                                         (menu-item 'm-stop "Stop counter")
                                         ))
                        ))

(define example-1-dialog%
  (class ww-webview-dialog%
    (inherit-field settings)
    (super-new [html-file dialog-html]
               [width 400]
               [height 300])

    (define/override (html-loaded)
      (super html-loaded)

      (ww-debug "html-loaded for example-1-dialog%")
      (let* ((btn (send this element 'ok-btn)))
        (send btn connect 'click (λ (data)
                                   (send this close))))

      (let* ((inp1 (send this element 'inp1))
             (inp2 (send this element 'inp2))
             (inp3 (send this element 'inp3)))
        (send inp1 set! (send settings get 'inp1 "<input 1 not set yet>"))
        (send inp2 set! (send settings get 'inp2 "<input 2 not set yet>"))
        (send inp3 set! (send settings get 'inp3 "<input 3 not set yet>"))
        (send inp1 on-change!
              (λ (val)
                (send settings set! 'inp1 val)))
        (send inp2 on-change! (λ (val) (send settings set! 'inp2 val)))
        (send inp3 on-change! (λ (val) (send settings set! 'inp3 val)))
        )
      )
    )
  )

(define-syntax inc
  (syntax-rules ()
    ((_ var)
     (λ ()
       (set! var (+ var 1))
       var))))

(define example-1-window%
  (class ww-webview%

    (inherit-field settings)
    (super-new [html-file html-start]
               )
    
    (define go-on-counter #f)
    (define c-counter 0)
    (define counter-inc 1)
    (define counter-thread #f)
    (define div-counter #f)
    (define my-dir (send settings get 'folder "."))


    (define/override (can-close?)
      (eq? counter-thread #f))

    (define/public (reset-counter)
      (set! go-on-counter #f)
      (set! counter-thread #f)
      )

    (define/public (inc-counter)
      (set! c-counter (+ c-counter counter-inc))
      (when (>= c-counter 1000)
        (set! counter-inc -1))
      (when (<= c-counter 0)
        (set! counter-inc 1))
      (send this update-counter))

    (define/public (update-counter)
      (send div-counter set-inner-html! (format "Count = ~a" c-counter))
      (when (and (> c-counter 0) (<= c-counter 100))
        (send div-counter set-style!
              (css-style '((background white)))))
      (when (and (> c-counter 100) (<= c-counter 200))
        (send div-counter set-style!
              (css-style '((background green) (color white)))))
      (when (and (> c-counter 200) (<= c-counter 300))
        (send div-counter set-style!
              (css-style '((background yellow) (font-size: 120%)))))
      (when (and (> c-counter 300) (<= c-counter 400))
        (send div-counter set-style!
              (css-style '((color white) (background orange) (font-size 110%)))))
      (when (and (> c-counter 400))
        (send div-counter set-style!
              (css-style '((color white) (background red) (font-size 120%) (font-weight bold)))))
      )

    (define/public (start-counter)
      (set! counter-thread
            (thread
             (λ ()
               (letrec ((f (λ ()
                             (when go-on-counter
                               (send this inc-counter)
                               (sleep 0.01)
                               (f)))))
                 (set! go-on-counter #t)
                 (f)))))
      )

    (define/public (set-folder new-dir)
      (set! my-dir new-dir)
      (send settings set 'folder new-dir)
      (let ((el (send this element 'folder)))
        (send el set-inner-html! (format "Selected folder: <b>~a</b>" my-dir))
        )
      )

    (define/override (choose-dir)
      (let ((new-dir (super choose-dir "Select a folder" my-dir)))
        (unless (eq? new-dir #f)
          (send this set-folder new-dir))))

    (define/public (prefs)
      (new example-1-dialog% [parent this] [settings (send this clone-settings 'example-1-dialog)]))

    (define/override (handle-navigate url type kind)
      (send this reset-counter)
      (super handle-navigate url type kind))

    (define/override (html-loaded)
      (ww-debug "HTML LOADED")
      (super html-loaded)

      (set! div-counter (send this element 'div-counter))
      (send this update-counter)
      (send this set-folder my-dir)

      (ww-debug "CONNECTING BUTTONS")
      (let* ((dialog-btn (send this element 'dialog-button))
             (start-stop-btn (send this element 'start-stop-button))
             (choose-dir-btn (send this element 'select-dir-button))
             )
        (send dialog-btn connect 'click
              (λ (data) (send this prefs)))
        
        (send start-stop-btn connect 'click
              (λ (data)
                (if (eq? counter-thread #f)
                    (begin
                      (send this start-counter)
                      (send start-stop-btn set-inner-html! "Stop Counter"))
                    (begin
                      (send this reset-counter)
                      (send start-stop-btn set-inner-html! "Start Counter"))
                    )
                )
              )
        (send choose-dir-btn connect 'click
              (λ (data)
                (send this choose-dir)))
        )
      
      (ww-debug "SETTING MENU")
      (let* ((div-open (send this element 'div-open))
             (c-open 0)
             (div-close (send this element 'div-close))
             (c-close 0)
             (div-copy (send this element 'div-copy))
             (c-copy 0)
             (div-cut (send this element 'div-cut))
             (c-cut 0)
             (div-paste (send this element 'div-paste))
             (c-paste 0)
             )
        
        (send this set-menu! test-menu)
        (send this connect-menu! 'm-quit
              (λ ()
                (send this reset-counter)
                (send this close))
              )
        (let ((make-menu-executor (λ (item elem string count)
                                    (send this connect-menu! item
                                          (λ ()
                                            (send elem set-inner-html! (format "~a ~a" string (count)))))))
              )
          (make-menu-executor 'm-open div-open "Open file" (inc c-open))
          (make-menu-executor 'm-close div-close "Close file" (inc c-close))
          (make-menu-executor 'm-copy div-copy "Edit Copy" (inc c-copy))
          (make-menu-executor 'm-cut div-cut "Edit Cut" (inc c-cut))
          (make-menu-executor 'm-paste div-paste "Edit Paste" (inc c-paste))

          (send this connect-menu! 'm-start
                (λ () (send this start-counter)))
          
          (send this connect-menu! 'm-stop
                (λ () (send this reset-counter)))

          (send this connect-menu! 'm-prefs
                (λ () (send this prefs)))

          (send this connect-menu! 'm-select-dir
                (λ () (send this choose-dir)))
                                     
          )
        )
      )

    (begin
      (displayln "Yes this works!")
      )
    )
  )
    
(define (run-example)
  (let* ((ini (new ini% [file 'web-racket-example1]))
         (settings (new ww-simple-ini% [ini ini] [section 'example-1-window]))
         (window (new example-1-window% [settings settings]))
         )
    window))
