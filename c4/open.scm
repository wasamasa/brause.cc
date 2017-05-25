(use (only http-client with-input-from-request)
     (only medea read-json)
     (only data-structures alist-ref o)
     (only ports with-output-to-string)
     (only sxml-transforms SRV:send-reply pre-post-order
           universal-conversion-rules)
     (only doctype doctype-rules)
     (only posix set-buffering-mode!)
     (only intarweb request-method request-uri)
     (only uri-common uri-path)
     spiffy)

(define api-url "https://api.koeln.ccc.de/")

(define (c4-state)
  (let* ((response (with-input-from-request api-url #f read-json))
         (state (alist-ref 'state response)))
    (list (alist-ref 'open state) (alist-ref 'lastchange state))))

(define (approximate-duration delta)
  (let* ((->int (o inexact->exact floor))
         (minute 60)
         (hour (* 60 minute))
         (day (* 24 hour))
         (month (* 30 day))
         (year (* 365 day))
         (duration
          (cond
           ((>= delta (* 2 year))
            (format "~a Jahren" (->int (/ delta year))))
           ((>= delta year)
            "einem Jahr")
           ((>= delta (* 2 month))
            (format "~a Monaten" (->int (/ delta month))))
           ((>= delta month)
            "einem Monat")
           ((>= delta (* 2 day))
            (format "~a Tagen" (->int (/ delta day))))
           ((>= delta day)
            "einem Tag")
           ((>= delta (* 2 hour))
            (format "~a Stunden" (->int (/ delta hour))))
           ((>= delta hour)
            "einer Stunde")
           ((>= delta (* 2 minute))
            (format "~a Minuten" (->int (/ delta minute))))
           ((>= delta minute)
            "einer Minute")
           ((= delta 1)
            "einer Sekunde")
           (else ; zero and more than one second are plural forms
            (format "~a Sekunden" (->int delta))))))
    (string-append "Seit etwa " duration)))

(define (status-page open? timestamp)
  (with-output-to-string
    (lambda ()
      (SRV:send-reply
       (pre-post-order
        `((doctype-html)
          (html (@ (lang "de"))
                (head
                 (meta (@ (charset "utf-8")))
                 (meta (@ (name "viewport")
                          (content ,(string-append "initial-scale=1.0,"
                                                   "width=device-width,"
                                                   "user-scalable=no"))))
                 (title "Hat der C4 offen?")
                 (link (@ (href "style.css")
                          (rel "stylesheet")
                          (type "text/css"))))
                (body
                 (h1 ,(if open? "Ja" "Nein"))
                 ,(if open?
                      `(h2 ,(approximate-duration (- (current-seconds)
                                                     timestamp)))
                      #f))))
        (append doctype-rules universal-conversion-rules))))))

(define (handle-request continue)
  (let* ((request (current-request))
         (method (request-method request))
         (path (uri-path (request-uri request))))
    (if (and (eq? method 'GET) (equal? path '(/ "")))
        (with-headers '((cache-control "no-cache"))
          (lambda () (send-response body: (apply status-page (c4-state)))))
        (continue))))

(define (main)
  (trusted-proxies '("127.0.0.1"))
  (vhost-map `((".*" . ,handle-request)))
  (root-path "static/")
  (server-bind-address "127.0.0.1")
  (server-port 8002)
  (set-buffering-mode! (current-output-port) #:line)
  (access-log (current-output-port))
  (start-server))

(main)
