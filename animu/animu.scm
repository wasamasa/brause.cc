(use srfi-1 srfi-18 http-client sendfile z3 ssax sxpath
     sql-de-lite rfc3339 atom matchable)

(define api-base "http://api.anidb.net:9001/httpapi")
;; for some reason registering an account and client isn't enough,
;; therefore I've stolen another client's ID and version
(define client-id "marf")
(define client-version 1)

(define failed? #f)

(define (fetch-episodes! path aid)
  (let ((url (format "~a?request=anime&client=~a&clientver=~a&protover=1&aid=~a"
                     api-base client-id client-version aid)))
    (call-with-input-request url #f (lambda (in)
                                      (call-with-output-file path
                                        (lambda (out)
                                          (sendfile in out)))))))

(define (episode-data episode)
  (list ((sxpath "string(/airdate)") episode)
        (string->number ((sxpath "string(/epno)") episode))
        (string->number ((sxpath "string(/@id)") episode))))

(define (make-error location message . condition)
  (let ((base (make-property-condition 'exn 'location location 'message message))
        (extra (apply make-property-condition condition)))
    (make-composite-condition base extra)))

(define (sxml-error data location)
  (abort (make-error location "invalid sxml" 'sxml 'data data)))

(define (response->metadata path)
  (let* ((out (z3:open-compressed-input-file path))
         (sxml (ssax:xml->sxml out '())))
    (close-output-port out)
    (if (null? ((sxpath "/anime") sxml))
        (sxml-error sxml 'response->metadata)
        (let* ((title ((sxpath "string(/anime/titles/title[@type='main'])") sxml))
               (episodes ((sxpath "/anime/episodes/episode[epno[@type='1']]") sxml))
               (metadata (map episode-data episodes))
               (today (time->string (seconds->utc-time) "%Y-%m-%d"))
               (metadata (filter (lambda (item) (string<=? (car item) today))
                                 metadata)))
          (cons title (sort metadata (lambda (a b) (< (cadr a) (cadr b)))))))))

(define (insert-metadata! db-path path aid)
  (define statement
    "INSERT OR REPLACE INTO episodes(aid, title, airdate, epno, epid)
     VALUES(?, ?, ?, ?, ?);")
  (match-let (((title . rows) (response->metadata path)))
    (call-with-database db-path
      (lambda (db)
        (with-transaction db
          (lambda ()
            (for-each
             (lambda (row)
               (apply exec (sql db statement) aid title row))
             rows)))))))

(define (save-all-metadata! db-path aids)
  (for-each
   (lambda (aid)
     (let ((file (create-temporary-file "gz")))
       (condition-case
        (begin
          (fetch-episodes! file aid)
          (insert-metadata! db-path file aid)
          (delete-file file))
        (ex (exn sxml)
            (let ((sxml ((condition-property-accessor 'sxml 'data) ex)))
              (print "failed fetching data for " aid)
              (pp sxml)
              (set! failed? #t))))
       (thread-sleep! 3)))
   aids))

(define (updated-at db)
  (define statement
    "SELECT airdate FROM episodes ORDER BY airdate DESC LIMIT 1;")
  (query fetch-value (sql db statement)))

(define (latest-posts db limit)
  (define statement
    "SELECT title, airdate, epno, epid FROM episodes
      ORDER BY airdate DESC, aid LIMIT ?")
  (query fetch-all (sql db statement) limit))

(define (datestring->datetime string)
  (rfc3339->string (time->rfc3339 (string->time string "%Y-%m-%d"))))

(define (permalink epid)
  (format "http://anidb.net/perl-bin/animedb.pl?show=ep&eid=~a" epid))

(define atom-limit 15)

(define (save-atom-feed! db-path path)
  (call-with-output-file path
    (lambda (out)
      (call-with-database db-path
        (lambda (db)
          (write-atom-doc
           (make-atom-doc
            (make-feed
             title: (make-title "New anime episodes")
             id: "http://animu.brause.cc/"
             updated: (datestring->datetime (updated-at db))
             authors: (list (make-author name: "anidb"))
             links: (list (make-link uri: "http://anidb.net"))
             entries: (map
                       (match-lambda
                        ((title airdate epno epid)
                         (make-entry
                          id: (format "anidb:~a" epid)
                          title: (make-title (format "~a #~a" title epno))
                          updated: (datestring->datetime airdate)
                          links: (list (make-link uri: (permalink epid))))))
                       (latest-posts db atom-limit))))
           out))))))

(define (main aids-path db-path feed-path)
  (let ((aids (call-with-input-file aids-path read)))
    (save-all-metadata! db-path aids)
    (save-atom-feed! db-path feed-path)
    (when failed?
      (exit 1))))

(apply main (command-line-arguments))
