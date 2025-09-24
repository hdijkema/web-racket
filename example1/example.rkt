#lang racket/gui


 (define m (menu (menu-item 'm-file "File"
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
                                   ))))