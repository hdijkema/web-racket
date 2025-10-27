#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/atomic
         setup/dirs
         "../utils/utils.rkt"
         (rename-in racket/gui
                    (-> %->))
         data/queue
         )

(provide webwire-new
         webwire-current
         webwire-id
         webwire-destroy
         webwire-command
         webwire-items
         webwire-items-available
         webwire-handlers!
         webwire-get
         webwire-status
         webwire-status->string
         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle finalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define webwire-will (make-will-executor))
;(void (thread (λ ()
;                (let loop ()
;                  (begin
;                    (sleep 0.1)
;                    (will-execute webwire-will) (loop))))))


(define-ffi-definer define-libwebui-wire
  (ffi-lib ;"c:/devel/racket/webui-wire/build/Debug/libwebui-wire.dll"
           "/home/hans/src/racket/webui-wire/build/Debug/liblibwebui-wire.so"
           #:custodian (current-custodian)))
  ;(ffi-lib "libwebui-wire" '("3" "4" "5" #f)
  ;         #:get-lib-dirs (lambda ()
  ;                          (cons (build-path ".") (get-lib-search-dirs)))
  ;         #:fail (lambda ()
  ;                  (ffi-lib (get-lib-path "libwebui-wire.dll")))
  ;         ))

(define-cpointer-type _webui-handle #:tag 'webui-handle)
;(define _webui-handle _pointer)

(define _webui-get-result
 (_enum '(null = 0
          event
          log
          invalid-handle = 256
          )))

(define _webui-handle-status
  (_enum '(valid = 1
           handle-destroyed
           handle-needs-destroying
           null-handle
           existing-handle-destroy-this-one
           handle-invalid-unexpected
           )))

(define-libwebui-wire webwire-new
  (_fun -> (handle : _webui-handle/null)
        -> (begin
             (unless (eq? handle #f)
               (will-register webwire-will
                            handle (λ (handle)
                                     (webwire-destroy handle))))
             (start-gui-processing)
             handle))
  #:c-id webwire_new)


(define-libwebui-wire webwire-current
  (_fun -> (handle : _webui-handle/null)
        -> (begin
             (unless (eq? handle #f)
               (will-register webwire-will
                            handle (λ (handle)
                                     (webwire-destroy handle)))
               (start-gui-processing)
               )
             handle))
  #:c-id webwire_current)

(define-libwebui-wire webwire-destroy
  (_fun _webui-handle/null -> _void
        -> (stop-gui-processing))
  #:c-id webwire_destroy)

(define-libwebui-wire webwire-command
  (_fun _webui-handle/null _string*/utf-8
        -> [r : _string*/utf-8]
        -> r
        )
  #:c-id webwire_command)

(define-libwebui-wire webwire-items
  (_fun _webui-handle/null -> _uint)
  #:c-id webwire_items)

(define-libwebui-wire webwire-get
  (_fun _webui-handle/null
        [evt : (_ptr o _string*/utf-8)]
        [kind : (_ptr o _string*/utf-8)]
        [msg : (_ptr o _string*/utf-8)]
        -> [ result : _webui-get-result ]
        -> (list result
                 evt
                 kind
                 msg)
        )
  #:c-id webwire_get)

(define-libwebui-wire webwire-id
  (_fun _webui-handle/null
        -> _int)
  #:c-id webwire_handle_id)

(define my-count 0)
(define last-queue-count -1)
(define (f c) (set! my-count (+ my-count 1)) (set! last-queue-count c))

(define (appl thunk)
  (thunk))
  
(define-libwebui-wire webwire-items-available
  (_fun _webui-handle/null (_cprocedure (list _int) _void #:async-apply appl) -> _void)
  #:c-id webwire_set_signaller)

(define-libwebui-wire webwire-handlers!
  (_fun _webui-handle/null
        (_fun #:async-apply appl _bytes/nul-terminated -> _void)
        (_fun #:async-apply appl _bytes/nul-terminated _bytes/nul-terminated -> _void)
        -> _bool)
  #:c-id webwire_set_handlers)

(define-libwebui-wire webwire-status
  (_fun _webui-handle/null -> _webui-handle-status)
  #:c-id webwire_status
  )

(define-libwebui-wire webwire-status->string
  (_fun _webui-handle-status
        -> (r : _string*/utf-8)
        -> r)
  #:c-id webwire_status_string)

(define-libwebui-wire webwire-process-gui
  (_fun _webui-handle/null -> _bool)
  #:c-id webwire_process_gui)


;(define (webwire-new evt-cb log-cb)
;  (parameterize ([g:current-eventspace (g:current-eventspace)])
;    (let ((evtcb (lambda (msg)
;                   (g:queue-callback (lambda () (evt-cb msg)))))
;          (logcb  (lambda (k m)
;                    (g:queue-callback (lambda () (log-cb k m)))))
;          )
;      (webwire_new evtcb logcb))))

(define last-evt #f)

(define (evt msg)
  (displayln msg)
  (set! last-evt msg))

(define last-log #f)

(define (log k m)
  (let ((msg (format "~a ~a" k m)))
    (displayln msg)
    (set! last-log msg)))

(define (reader h)
  (let ((l (webwire-get h)))
    (let ((result (car l)))
      (unless (or (eq? result 'null) (eq? result 'invalid-handle))
        (let* ((evt (cadr l))
               (kind (caddr l))
               (msg (cadddr l)))
          (unless (eq? evt #f)
            (displayln (format "EVENT:~a" evt)))
          (unless (eq? kind #f)
            (displayln (format "~a:~a" kind msg)))
          (reader h)))
      result)))
  
(define (reader-thread h)
    (thread (lambda ()
              (letrec ((f (lambda ()
                            (let ((r (reader h)))
                              (sleep 0.01);
                              ;(displayln r)
                              (if (eq? r 'invalid-handle)
                                  r
                                  (f))))))
                (f)))))


(define evt-fifo (make-queue))
(define log-fifo (make-queue))


(define (qthread)
  (parameterize ([current-eventspace (current-eventspace)])
    (thread
     (lambda ()
       (letrec ((f (lambda ()
                     (if (> (queue-length evt-fifo) 0)
                         (let ((e (dequeue! evt-fifo)))
                           (queue-callback (lambda ()
                                             (display e)
                                             (display " - ")
                                             (displayln (current-thread))))
                           (yield)
                           (f))
                         (begin
                           ;(displayln 'sleeping)
                           (sleep 0.005)
                           (f))))))
         (f))))
    ))

;(define (h-evt evt)
;  (enqueue! evt-fifo evt))

(define ce (current-eventspace))

(define h-evt (parameterize ([current-eventspace ce])
                (lambda (evt)
                  (queue-callback (lambda ()
                                      (enqueue! evt-fifo
                                                (list evt (current-thread)))))
                  (yield)))
  )

(define h-log  (parameterize ([current-eventspace ce])
                (lambda (kind msg)
                  (queue-callback (lambda ()
                                      (enqueue! log-fifo
                                                (list kind msg (current-thread)))))
                  (yield)))
  )
  
;  (enqueue! log-fifo (list kind msg (current-thread))))

(define (h-ffi-evt evt)
  (h-evt evt))
;  (parameterize ([g:current-eventspace (g:current-eventspace)])
;                    (lambda (evt) 
;                      (g:queue-callback (lambda () (h-evt evt))))))

(define (h-ffi-log kind msg)
  (h-log kind msg))

;  (parameterize ([g:current-eventspace (g:current-eventspace)])
;                    (lambda (kind msg) 
;                      (g:queue-callback (lambda () (h-log kind msg))))))


; Make sure GUI Events are processed (e.g. for linux - gtk main loop)

(define gui-processing-thread #f)
(define gui-processing-go-on #f)
  
(define (start-gui-processing)
  (when (eq? gui-processing-thread #f)
    (set! gui-processing-go-on #t)
    (set! gui-processing-thread (thread (λ ()
                                          (begin
                                            (displayln "gui processing starts")
                                            (letrec ((loop (λ ()
                                                             (sleep 0.05)
                                                             (let ((processed (webwire-process-gui #f)))
                                                               (when (or gui-processing-go-on processed)
                                                                 (loop))))))
                                              (loop))
                                            (displayln "gui processing stops")
                                            (set! gui-processing-thread #f)))))
    )
  )

(define (stop-gui-processing)
  (void (thread (λ ()
                  (display "waiting 5 seconds")
                  (sleep 5)
                  (displayln "stopping gui processing")
                  (set! gui-processing-go-on #f)))))

  