#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/atomic
         setup/dirs
         "../utils/utils.rkt"
         (prefix-in g: racket/gui)
         )

(provide webwire-new
         webwire-current
         webwire-destroy
         webwire-command
         webwire-items
         webwire-get
         webwire-status
         webwire-status->string
         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle finalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define webwire-will (make-will-executor))
(void (thread (λ () (let loop () (will-execute webwire-will) (loop)))))


(define-ffi-definer define-libwebui-wire
  (ffi-lib "c:/devel/racket/webui-wire/build/Debug/libwebui-wire.dll"
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
             handle))
  #:c-id webwire_new)

(define-libwebui-wire webwire-current
  (_fun -> (handle : _webui-handle/null)
        -> (begin
             (unless (eq? handle #f)
               (will-register webwire-will
                            handle (λ (handle)
                                     (webwire-destroy handle))))
             handle))
  #:c-id webwire_current)

(define-libwebui-wire webwire-destroy
  (_fun _webui-handle/null -> _void)
  #:c-id webwire_destroy)

(define-libwebui-wire webwire-command
  (_fun _webui-handle/null _string/utf-8
        -> [r : _string/utf-8]
        -> r
        )
  #:c-id webwire_command)

(define-libwebui-wire webwire-items
  (_fun _webui-handle/null -> _uint)
  #:c-id webwire_items)

(define-libwebui-wire webwire-get
  (_fun _webui-handle/null
        [evt : (_ptr o _string/utf-8)]
        [kind : (_ptr o _string/utf-8)]
        [msg : (_ptr o _string/utf-8)]
        -> [ result : _webui-get-result ]
        -> (list result
                 evt
                 kind
                 msg)
        )
  #:c-id webwire_get)

#|
                 (if (eq? evt #f)
                     #f
                     (cast evt _pointer _string/utf-8))
                 (if (eq? kind #f)
                     #f
                     (string->symbol
                      (cast kind _pointer _string/utf-8)))
                 (if (eq? msg #f)
                     #f
                     (cast msg _pointer _string/utf-8))
                 )
|#

(define-libwebui-wire webwire-status
  (_fun _webui-handle/null -> _webui-handle-status)
  #:c-id webwire_status
  )

(define-libwebui-wire webwire-status->string
  (_fun _webui-handle-status
        -> (r : _string/utf-8)
        -> r)
  #:c-id webwire_status_string)






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

