(module ww-time racket/base

  (require "../utils/sprintf.rkt")

  (define-struct time
    (second minute hour time-zone-offset) #:transparent)

  (define (time-for-time-zone-offset t time-zone-offset)
    (let* ((tzo-t (time-time-zone-offset t))
           (tzo-d time-zone-offset))
      (

  (define (time-for-date dt t)
    (make-date (time-second t) (time-minute t) (time-hour t)
               (date-day dt) (date-month dt) (date-year dt)
               (date-week-day dt) (date-year-day dt)
               (date-dst? dt) (date-time-zone-offset )))

  (define (->time date_or_time)
    (if (time? date_or_time)
        date_or_time
        (if (date? date_or_time)
            (time-of-date date_or_time)
            (error "data? or time? expected"))))

  (define (time-of-date dt)
    (make-time (date-second dt) (date-minute dt) (date-hour dt)))

  (define (time=? time_or_date_1 time_or_date_2)
    (let ((t1 (->time time_or_date_1))
          (t2 (->time time_or_date_2)))
      (and
       (= (time-hour t1) (time-hour t2))
       (= (time-minute t1) (time-minute t2))
       (= (time-second t1) (time-second t2)))))

  (define (time>? time_or_date_1 time_or_date_2)
    (let ((t1 (->time time_or_date_1))
          (t2 (->time time_or_date_2)))
      (if (> (time-hour t1) (time-hour t2))
          #t
          (if (= (time-hour t1) (time-hour t2))
              (if (> (time-minute t1) (time-minute t2))
                  #t
                  (if (= (time-minute t1) (time-minute t2))
                      (> (time-second t1) (time-second t2))
                      #f
                      )
                  )
              #f)
          )
      )
    )

  (define (time>=? a b)
    (or (time>? a b) (time=? a b)))

  (define (time<? a b)
    (not (time>=? a b)))

  (define (time<=? a b)
    (not (time>? a b)))
    
  (define (current-time)
    (->time (seconds->date (current-seconds))))

  (define re-time #px"([0-9]{1,2})[:]([0-9]{1,2})([:]([0-9]{1,2})){0,1}")
  
  (define (string->time str)
    (let ((m (regexp-match re-time str)))
      (when (eq? m #t)
        (error "time not recognized, should be '~H:~M' or '~H:~M:~S'"))
      (let* ((h* (list-ref m 1))
             (m* (list-ref m 2))
             (s* (list-ref m 4))
             (h (string->number h*))
             (m (string->number m*))
             (s (if (eq? s* #f) 0 (string->number s*))))
        (when (> h 23)
          (error "hour > 23"))
        (when (> m 59)
          (error "minute > 59"))
        (when (> s 59)
          (error "second > 59"))
        (make-time s m h))))

  (define (time->string t . seconds)
    (let ((do-seconds (if (null? seconds) #t (car seconds))))
      (if (eq? do-seconds #t)
          (sprintf "%02d:%02d:%02d" (time-hour t) (time-minute t) (time-second t))
          (sprintf "$02d:%02d" (time-hour t) (time-minute t)))))

  ); end of module
