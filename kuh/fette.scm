(import scheme)
(import (chicken base))
(import (chicken port))
(import atom)
(import format)
(import intarweb)
(import matchable)
(import rfc3339)
(import spiffy)
(import sql-de-lite)
(import sxml-serializer)
(import uri-common)

(define db (open-database "db.sqlite3"))

(define news-items 10)

(define (fetch-latest)
  (query fetch-all
         (sql db "SELECT * FROM burgers ORDER BY date DESC LIMIT ?;")
         news-items))

(define (updated-at)
  (query fetch-value
         (sql db "SELECT date FROM burgers ORDER BY date DESC LIMIT 1;")))

(define (unix->datetime seconds)
  (rfc3339->string (seconds->rfc3339 seconds)))

(define (unix->date seconds)
  (let ((record (seconds->rfc3339 seconds)))
    (format "~4d-~2,'0d-~2,'0d"
            (rfc3339-year record)
            (rfc3339-month record)
            (rfc3339-day record))))

(define (feed-item url description)
  (serialize-sxml
   `(div
     (p (img (@ (src ,url))))
     (p ,description))))

(define (feed)
  (write-atom-doc
   (make-atom-doc
    (make-feed
     title: (make-title "Fette Brause")
     id: "https://fette.brause.cc/"
     updated: (unix->datetime (updated-at))
     authors: (list (make-author name: "Vasilij Schneidermann"))
     links: (list (make-link uri: "https://facebook.com/DiefetteKuh"))
     entries: (map (match-lambda
                    ((id date description url permalink)
                     (make-entry
                      id: permalink
                      title: (make-title (unix->date date))
                      updated: (unix->datetime date)
                      links: (list (make-link uri: permalink))
                      content: (make-content (feed-item url description)
                                             type: 'html))))
                   (fetch-latest))))))

(define (handle-request continue)
  (let* ((request (current-request))
         (method (request-method request))
         (path (uri-path (request-uri request))))
    (if (equal? path '(/ ""))
        (cond
         ((eq? method 'HEAD)
          (send-response code: 200))
         ((eq? method 'GET)
          (send-response body: (with-output-to-string feed)
                         headers: '((content-type "application/xml"))))
         (else
          (continue)))
        (continue))))

(define (main)
  (trusted-proxies '("127.0.0.1"))
  (vhost-map `((".*" . ,handle-request)))
  (server-bind-address "127.0.0.1")
  (server-port 8001)
  (set-buffering-mode! (current-output-port) #:line)
  (access-log (current-output-port))
  (start-server))

(main)
