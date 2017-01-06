(use (only http-client with-input-from-request)
     (only medea read-json)
     (only posix set-buffering-mode!)
     (only intarweb request-method request-uri)
     (only uri-common uri-path)
     spiffy)

(define api-url "https://api.koeln.ccc.de/")

(define (open?)
  (let ((response (with-input-from-request api-url #f read-json)))
    (alist-ref 'open (alist-ref 'state response))))

(define (handle-request continue)
  (let* ((request (current-request))
         (method (request-method request))
         (path (uri-path (request-uri request))))
    (if (and (eq? method 'GET) (equal? path '(/ "")))
      (if (open?)
          (send-static-file "ja.html")
          (send-static-file "nein.html"))
      (continue))))

(define (main)
  (vhost-map `((".*" . ,handle-request)))
  (root-path "static/")
  (server-bind-address "127.0.0.1")
  (server-port 8002)
  (set-buffering-mode! (current-output-port) #:line)
  (access-log (current-output-port))
  (start-server))

(main)
