#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/atomic
         setup/dirs
         "../utils/utils.rkt"
         (prefix-in g: racket/gui)
         )

(provide webwire_new
         webwire_destroy
         webwire_command
         webwire_get
         webwire_status
         webwire_status_string
         reader)


(define-ffi-definer define-libwebui-wire
  (ffi-lib "c:/devel/racket/webui-wire/build/Debug/libwebui-wire.dll"
           #:custodian (current-custodian)))
  ;(ffi-lib "libwebui-wire" '("3" "4" "5" #f)
  ;         #:get-lib-dirs (lambda ()
  ;                          (cons (build-path ".") (get-lib-search-dirs)))
  ;         #:fail (lambda ()
  ;                  (ffi-lib (get-lib-path "libwebui-wire.dll")))
  ;         ))



(define _webui-handle _pointer)

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

(define-libwebui-wire webwire_new
  (_fun -> _webui-handle))

(define-libwebui-wire webwire_destroy
  (_fun _webui-handle -> _void))

(define-libwebui-wire webwire_command
  (_fun _webui-handle _string/utf-8 -> _string/utf-8))

(define-libwebui-wire webwire_get
  (_fun _webui-handle
        [evt : (_ptr o _string/utf-8)]
        [kind : (_ptr o _string/utf-8)]
        [msg : (_ptr o _string/utf-8)]
        -> [ result : _webui-get-result ]
        -> (list result evt kind msg)))

(define-libwebui-wire webwire_status
  (_fun _webui-handle -> _webui-handle-status))

(define-libwebui-wire webwire_status_string
  (_fun _webui-handle-status -> _string/utf-8))




(define (webwire-new evt-cb log-cb)
  (parameterize ([g:current-eventspace (g:current-eventspace)])
    (let ((evtcb (lambda (msg)
                   (g:queue-callback (lambda () (evt-cb msg)))))
          (logcb  (lambda (k m)
                    (g:queue-callback (lambda () (log-cb k m)))))
          )
      (webwire_new evtcb logcb))))

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
  (let ((l (webwire_get h)))
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
  
