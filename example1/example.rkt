#lang racket/gui

(require "../main.rkt"
         racket/runtime-path
         racket/gui
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

(displayln html-start)
(displayln dialog-html)


(define test-menu (menu (menu-item 'm-file "File"
                                   #:submenu
                                   (menu (menu-item 'm-open "Open File")
                                         (menu-item 'm-close "Close File")
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
  (class ww-webview%
    (super-new [html-file dialog-html]
               [width 400]
               [height 300])

    (define/override (html-loaded)
      (super html-loaded)
      (ww-debug "html-loaded for example-1-dialog%")
      (let* ((btn (send this element 'ok-btn)))
        (send btn connect 'click (λ (data)
                                   (send this close)))))
    ))

(define-syntax inc
  (syntax-rules ()
    ((_ var)
     (λ ()
       (set! var (+ var 1))
       var))))

(define example-1-window%
  (class ww-webview%
    (super-new [html-file (begin (displayln html-start) html-start)])

    (define/override (html-loaded)
      (ww-debug "HTML LOADED")
      (super html-loaded)

      (ww-debug "CONNECTING BUTTONS")
      (let* ((dialog-btn (send this element 'dialog-button))
             )
        (send dialog-btn connect 'click (λ (data)
                                           (new example-1-dialog% [parent this])))
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
             (div-counter (send this element 'div-counter))
             (go-on-counter #f)
             (c-counter 0)
             (counter-thread #f)
             )
        (send this set-menu! test-menu)
        (send this connect-menu! 'm-quit
              (λ ()
                (set! go-on-counter #f)
                (send this close))
              )
        (let ((make-menu-executor (λ (item elem string count)
                                    (send this connect-menu! item
                                          (λ ()
                                            (send elem set-inner-html (format "~a ~a" string (count)))))))
              )
          (make-menu-executor 'm-open div-open "Open file" (inc c-open))
          (make-menu-executor 'm-close div-close "Close file" (inc c-close))
          (make-menu-executor 'm-copy div-copy "Edit Copy" (inc c-copy))
          (make-menu-executor 'm-cut div-cut "Edit Cut" (inc c-cut))
          (make-menu-executor 'm-paste div-paste "Edit Paste" (inc c-paste))

          (send this connect-menu! 'm-start
                (λ ()
                  (set! counter-thread
                        (thread
                         (λ ()
                           (letrec ((f (λ ()
                                         (when go-on-counter
                                           (set! c-counter (+ c-counter 1))
                                           (send div-counter set-inner-html (format "Count = ~a" c-counter))
                                           (when (and (> c-counter 0) (<= c-counter 1))
                                             (send div-counter set-style!
                                                   (css-style '((background white)))))
                                           (when (and (> c-counter 100) (<= c-counter 101))
                                             (send div-counter set-style!
                                                   (css-style '((background green) (color white)))))
                                           (when (and (> c-counter 200) (<= c-counter 201))
                                             (send div-counter set-style!
                                                   (css-style '((background yellow) (font-size: 120%)))))
                                           (when (and (> c-counter 300) (<= c-counter 301))
                                             (send div-counter set-style!
                                                   (css-style '((color white) (background orange) (font-size 130%)))))
                                           (when (and (> c-counter 400) (<= c-counter 401))
                                             (send div-counter set-style!
                                                   (css-style '((color white) (background red) (font-size 150%) (font-weight bold)))))
                                           (sleep 0.01)
                                           (f)))))
                             (set! go-on-counter #t)
                             (f)))))))
          
          (send this connect-menu! 'm-stop
                (λ ()
                  (set! go-on-counter #f)
                  (set! c-counter 0)))

          (send this connect-menu! 'm-prefs (λ () (new example-1-dialog% [parent this])))
                                     
          )
        )
      )

    (begin
      (displayln html-start)
      )
    )
  )
    
