(use (only http-client with-input-from-request)
     (only uri-common form-urlencode)
     (only matchable match-let*)
     (only sql-de-lite open-database fetch-value
           sql schema exec query)
     (only medea json-parsers read-json)
     (only data-structures alist-ref)
     (only srfi-1 find)
     clojurian-syntax)

(define array-as-list-parser
  (cons 'array (lambda (x) x)))

(json-parsers (cons array-as-list-parser (json-parsers)))

(define (access-token filename)
  (let* ((credentials (with-input-from-file filename read))
         (app-id (alist-ref 'app-id credentials))
         (app-secret (alist-ref 'app-secret credentials)))
    (assert (and app-id app-secret) "App ID or secret not provided")
    (format "~a|~a" app-id app-secret)))

(define graph-api-base-url "https://graph.facebook.com/v2.8")

(define (graph-api-request path options token)
  (let* ((params (cons (cons 'access_token token) options))
         (query (form-urlencode params separator: "&"))
         (url (format "~a/~a?~a" graph-api-base-url path query)))
    (with-input-from-request url #f read-json)))

(define (timeline-album-id token)
  (->> token
       (graph-api-request "DiefetteKuh/albums" '())
       (alist-ref 'data)
       (find (lambda (album)
               (equal? (alist-ref 'name album)
                       "Timeline Photos")))
       (alist-ref 'id)))

(define (burger-photo-id album-id token)
  (->> token
       (graph-api-request (string-append album-id "/photos") '())
       (alist-ref 'data)
       (find (lambda (photo)
               (substring-index "BURGER DER WOCHE" (alist-ref 'name photo))))
       (alist-ref 'id)))

(define (burger-metadata photo-id token)
  (let* ((fields "created_time,name,images,link")
         (params `((fields . ,fields) (date_format . "U")))
         (data (graph-api-request photo-id params token))
         (timestamp (alist-ref 'created_time data))
         (description (alist-ref 'name data))
         (image (alist-ref 'source (car (alist-ref 'images data))))
         (permalink (alist-ref 'link data)))
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
  (let ((token (access-token "credentials.alist")))
    (-> (timeline-album-id token)
        (burger-photo-id token)
        (burger-metadata token)
        (update-database))))

(main)
