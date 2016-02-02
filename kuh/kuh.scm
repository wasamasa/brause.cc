(use (only http-client with-input-from-request)
     (only uri-common uri-encode-string)
     (only matchable match-let*)
     (only sql-de-lite open-database fetch-value fetch-all
           sql schema exec query)
     (only medea json-parsers json->string read-json)
     (only srfi-1 find))

(define array-as-list-parser
  (cons 'array (lambda (x) x)))

(json-parsers (cons array-as-list-parser (json-parsers)))

(define fql-api-base-url "https://graph.facebook.com/fql")

(define (page-albums)
  (let* ((params '((page_id . "SELECT page_id FROM page WHERE username = 'Diefettekuh'")
                   (albums . "SELECT aid, name FROM album WHERE owner IN (SELECT page_id FROM #page_id)")))
         (json (json->string params))
         (url (format "~a?q=~a" fql-api-base-url (uri-encode-string json)))
         (response (with-input-from-request url #f read-json)))
    (cadr (alist-ref 'data response))))

(define (timeline-album-id data)
  (let* ((needle "Timeline Photos")
         (haystack (alist-ref 'fql_result_set data))
         (match (find (lambda (item) (equal? (alist-ref 'name item) needle)) haystack)))
    (alist-ref 'aid match)))

(define (album-photos album-id)
  (let* ((query (format "SELECT created, caption, link, images FROM photo WHERE aid = '~a' LIMIT 10" album-id))
         (params `((photos . ,query)))
         (json (json->string params))
         (url (format "~a?q=~a" fql-api-base-url (uri-encode-string json)))
         (response (with-input-from-request url #f read-json)))
    (alist-ref 'fql_result_set (car (alist-ref 'data response)))))

(define (burger-metadata data)
  (let* ((needle "BURGER DER WOCHE")
         (match (find (lambda (item) (substring=? (alist-ref 'caption item) needle)) data))
         (timestamp (alist-ref 'created match))
         (description (alist-ref 'caption match))
         (image (alist-ref 'source (car (alist-ref 'images match))))
         (permalink (alist-ref 'link match)))
    (list timestamp description image permalink)))

(define db (open-database "db.sqlite3"))

(define (init-database)
  (if (null? (schema db))
      (exec (sql db "CREATE TABLE burgers(id INTEGER PRIMARY KEY,
                                          date NUMBER,
                                          description TEXT,
                                          url TEXT,
                                          permalink TEXT)"))))

(define (update-database data)
  (match-let* (((date description url permalink) data)
               (latest (query fetch-value (sql db "SELECT date FROM burgers
                                                               WHERE date = ?;")
                              date)))
    (if (not latest)
        (exec (sql db "INSERT INTO burgers(date, description, url, permalink)
                              VALUES(?, ?, ?, ?);")
              date description url permalink))))

(define (main)
  (init-database)
  (update-database (burger-metadata (album-photos (timeline-album-id (page-albums))))))

(main)
