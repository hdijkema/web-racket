(module web-wire racket/base

  (require racket/system
           )

  (provide ww-start
           ww
           ww-stop
           
           ww-new
           ww-move
           ww-resize
           ww-set-title
           ww-set-icon
   )

  (define current-win-release "

  (define ww-thread #f);
  

  (define (ww-start)
    (if (eq? ww-thread #f)
        (let ((cwd (current-directory)
    (let* ((cwd (current-directory))
           (bin (build-path cwd ".." "bin" "linux" "web-wire"))
           (ww  (make-ww))
           (ports (process bin)))
      (let* ((in (car ports))
             (out (cadr ports))
             (pid (caddr ports))
             (err (cadddr ports))
             )
        (set-ww-process-handler! (thread 
      (displayln bin)
      (let ((ports (process bin)))
        
      #t))

  (define (ww-stop)
    #t)

  (define (ww-new)
    #t)

  (define (ww-move)
    #t)

  (define (ww-resize)
    #t)

  (define (ww-set-title)
    #t)

  (define (ww-set-icon)
    #t)

  ); end of module