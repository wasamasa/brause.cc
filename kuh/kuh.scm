(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken sort))
(import (chicken string))
(import (srfi 1))
(import (clojurian syntax))
(import http-client)
(import matchable)
(import medea)
(import sql-de-lite)
(import uri-common)

(define array-as-list-parser
  (cons 'array (lambda (x) x)))

(json-parsers (cons array-as-list-parser (json-parsers)))

(define (access-token filename)
  (let* ((credentials (with-input-from-file filename read))
         (app-id (alist-ref 'app-id credentials))
         (app-secret (alist-ref 'app-secret credentials)))
    (assert (and app-id app-secret) "App ID or secret not provided")
    (format "~a|~a" app-id app-secret)))

(define graph-api-base-url "https://graph.facebook.com/v2.10")

(define (graph-api-request path options token)
  (let* ((params (cons (cons 'access_token token) options))
         (query (form-urlencode params separator: "&"))
         (url (format "~a/~a?~a" graph-api-base-url path query)))
    (with-input-from-request url #f read-json)))

(define (burger-photo-id token)
  (->> token
       (graph-api-request "DiefetteKuh/photos" '((type . uploaded)))
       (alist-ref 'data)
       (find (lambda (photo)
               (let ((name (alist-ref 'name photo)))
                 (and name (substring-index-ci "BURGER DER WOCHE" name)))))
       (alist-ref 'id)))

(define (image-size image)
  (* (alist-ref 'height image)
     (alist-ref 'width image)))

(define (image-greater? a b)
  (> (image-size a) (image-size b)))

(define (pick-image images)
  (let ((max-size (* 600 800))
        (sorted (sort images image-greater?)))
    (car (filter (lambda (image) (< (image-size image) max-size)) sorted))))

(define (burger-metadata photo-id token)
  (let* ((fields "created_time,name,images,link")
         (params `((fields . ,fields) (date_format . "U")))
         (data (graph-api-request photo-id params token))
         (timestamp (alist-ref 'created_time data))
         (description (alist-ref 'name data))
         (image (pick-image (alist-ref 'images data)))
         (image-link (alist-ref 'source image))
         (permalink (alist-ref 'link data)))
    (list timestamp description image-link permalink)))

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
    (-> (burger-photo-id token)
        (burger-metadata token)
        (update-database))))

(main)
