(module utils racket/base

  (provide while
           until
           get-lib-path
           do-for
           )

  (define-syntax while
    (syntax-rules ()
      ((_ cond body ...)
       (letrec ((while-f (lambda (last-result)
                           (if cond
                               (let ((last-result (begin
                                                    body
                                                    ...)))
                                 (while-f last-result))
                               last-result))))
         (while-f #f))
       )
      ))

  (define-syntax until
    (syntax-rules ()
      ((_ cond body ...)
       (letrec ((until-f (lambda (last-result)
                           (if cond
                               last-result
                               (let ((last-reult (begin
                                                   body
                                                   ...)))
                                 (until-f last-result))))))
         (until-f #f)))))

  (define-syntax do-for
    (syntax-rules ()
      ((_ (init cond next) body ...)
       (begin
         init
         (letrec ((do-for-f (lamba ()
                                   (if cond
                                       (begin
                                         (begin
                                           body
                                           ...)
                                         next
                                         (do-for-f))))))
           (do-for-f))))))

  (define (get-lib-path lib)
    (let ((platform (system-type)))
      (cond
        [(eq? platform 'windows) 
         (let ((try1 (build-path (current-directory) ".." "lib" "dll" lib))
               (try2 (build-path (current-directory) "lib" "dll" lib)))
           (if (file-exists? try1)
               try1
               try2)
           )]
        [else
         (error (format "Install the shared library: ~a" lib))]
        )))
                              

  ) ; end of module
