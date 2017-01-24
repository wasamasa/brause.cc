(use http-client html-parser sxpath medea
     irregex data-structures clojurian-syntax
     sql-de-lite rfc3339 matchable atom)

(define base-url "https://www.instagram.com/breadfaceblog/")

(define array-as-list-parser
  (cons 'array (lambda (x) x)))

(json-parsers (cons array-as-list-parser (json-parsers)))

(define (fetch-json)
  (let* ((document (with-input-from-request base-url #f html->sxml))
         ;; <script type="text/javascript">window._sharedData = ...</script>
         (query "string(//script[contains(string(),'window._sharedData')])")
         (js ((sxpath query) document))
         ;; window._sharedData = {...};
         (json-text (substring js 21 (sub1 (string-length js)))))
    (read-json json-text)))

(define (description->title description)
  ;; we don't need no stinkin' emoji
  (let* ((re (irregex '(: (+ (~ print)))))
         (title (car (irregex-split re description))))
    (string-chomp title " ")))

(define (posts json)
  (let ((entries (->> json
                      (alist-ref 'entry_data)
                      (alist-ref 'ProfilePage)
                      (car)
                      (alist-ref 'user)
                      (alist-ref 'media)
                      (alist-ref 'nodes))))
    (map (lambda (entry)
           `((permalink . ,(format "https://www.instagram.com/p/~a/"
                                   (alist-ref 'code entry)))
             (timestamp . ,(inexact->exact (alist-ref 'date entry)))
             (title . ,(description->title (alist-ref 'caption entry)))
             (description . ,(alist-ref 'caption entry))))
         entries)))

(define database "db.sqlite3")

(define (init-database db)
  (if (null? (schema db))
      (exec (sql db "CREATE TABLE posts(id INTEGER PRIMARY KEY,
                                        permalink TEXT,
                                        timestamp INTEGER,
                                        title TEXT,
                                        description TEXT)"))))

(define (fetch-known-timestamps db)
  (query fetch-column (sql db "SELECT timestamp FROM posts")))

(define (insert-post! db post)
  (exec (sql db "INSERT INTO posts(permalink, timestamp, title, description)
                 VALUES(?, ?, ?, ?)")
        (alist-ref 'permalink post)
        (alist-ref 'timestamp post)
        (alist-ref 'title post)
        (alist-ref 'description post)))

(define (insert-posts! db posts)
  (let* ((known-timestamps (fetch-known-timestamps db))
         (new-posts (remove (lambda (post)
                              (member (alist-ref 'timestamp post)
                                      known-timestamps))
                            posts)))
    (with-transaction db
      (lambda ()
        (for-each
         (lambda (post) (insert-post! db post))
         new-posts)))))

(define (updated-at db)
  (query fetch-value
         (sql db "SELECT timestamp
                  FROM posts
                  ORDER BY timestamp DESC
                  LIMIT 1;")))

(define (latest-posts db limit)
  (query fetch-all
         (sql db "SELECT permalink, timestamp, title, description
                  FROM posts
                  ORDER BY timestamp DESC
                  LIMIT ?")
         limit))

(define atom-file "atom.xml")
(define atom-limit 15)

(define (unix->datetime seconds)
  (rfc3339->string (seconds->rfc3339 seconds)))

(define (atom-feed db file posts)
  (write-atom-doc
   (make-atom-doc
    (make-feed
     title: (make-title "breadfaceblog")
     id: (format "http://brot.brause.cc/~a" file)
     updated: (unix->datetime (updated-at db))
     authors: (list (make-author name: "Bread Face" email: "breadfaceblog@gmail.com"))
     links: (list (make-link uri: "https://www.instagram.com/breadfaceblog/"))
     entries: (map
               (match-lambda
                ((permalink timestamp title description)
                 (make-entry
                  id: (format "breadfaceblog:~a" permalink)
                  title: (make-title title)
                  updated: (unix->datetime timestamp)
                  links: (list (make-link uri: permalink))
                  content: (make-content description))))
               posts)))))

(define (main)
  (let ((db (open-database database))
        (file atom-file))
    (init-database db)
    (insert-posts! db (posts (fetch-json)))
    (let ((posts (latest-posts db atom-limit)))
      (with-output-to-file (format "~a/~a" (cadr (argv)) file)
        (lambda () (atom-feed db file posts))))))

(main)
