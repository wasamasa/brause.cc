(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import (chicken time posix))
(import (srfi 1))
(import (srfi 18))
(import atom)
(import http-client)
(import intarweb)
(import matchable)
(import rfc3339)
(import sql-de-lite)
(import ssax)
(import sxpath)
(import sxpath-lolevel)
(import uri-common)

(form-urlencoded-separator "&&")

(define user-agent "Mozilla/5.0 (compatible; +http://animu.brause.cc)")
(define base-url "https://anidb.net/perl-bin/animedb.pl")
(define api-headers (headers '((X-LControl x-no-cache))))

(define str string-append)
(define (sqlite-bool x) (if x 1 0))
(define (sqlite-unbool x) (if (= x 1) #t #f))

(define (latest-data-for-anime db aid #!optional gid)
  (let ((q (str "SELECT eprange, completed FROM releases"
                " WHERE aid = ?" (if gid " AND gid = ?" "")
                " ORDER BY lastup DESC LIMIT 1")))
    (apply query fetch-alist (sql db q)
           (if gid (list aid gid) (list aid)))))

(define (add-release! db aid atitle gid gtitle eprange lastup lastep completed)
  (exec (sql db "INSERT OR REPLACE INTO releases
                 VALUES(?, ?, ?, ?, ?, ?, ?, ?)")
        aid atitle gid gtitle eprange
        lastup lastep (sqlite-bool completed)))

(define (read-xml in)
  (ssax:xml->sxml in '()))

(define (anime-xml aid)
  (let* ((params `((show . xml)
                   (t . anime)
                   (aid . ,aid)))
         (uri (update-uri (uri-reference base-url) query: params))
         (request (make-request method: 'GET uri: uri headers: api-headers))
         (xml (call-with-input-request request #f read-xml)))
    (when (null? ((sxpath "//root/animes/anime") xml))
      (error "Invalid SXML"))
    xml))

(define (sxml:value node #!optional (converter identity))
  (let ((value (sxml:string node)))
    (if (equal? value "")
        #f
        (converter value))))

(define (compact-eprange epcount)
  (if (= epcount 1) "1" (str "1-" (number->string epcount))))

(define (expand-eprange eprange)
  (append-map (lambda (range)
                (if (substring-index "-" range)
                    (let* ((ends (map string->number (string-split range "-")))
                           (start (car ends))
                           (end (cadr ends)))
                      (iota (add1 (- end start)) start))
                    (list (string->number range))))
              (string-split (or eprange "") ",")))

(define (check-for-group-release! db xml aid gid)
  (let* ((latest-data (latest-data-for-anime db aid gid))
         (old-eprange (alist-ref 'eprange latest-data))
         (completed? (alist-ref 'completed latest-data)))
    (when (not completed?)
      (let* ((atitle ((sxpath "string(//title[@type='main'])") xml))
             (epcount ((sxpath "number(//anime/neps)") xml))
             (group ((sxpath (format "//group[@id=~a]" gid)) xml))
             (new-eprange (sxml:value ((sxpath "eprange") group)))
             (gtitle (sxml:value ((sxpath "name") group)))
             (lastup (sxml:value ((sxpath "lastup") group)))
             (lastep (sxml:value ((sxpath "lastep") group) string->number)))
        (when (and new-eprange (not (equal? old-eprange new-eprange)))
          (let ((completed? (equal? new-eprange (compact-eprange epcount))))
            (add-release! db aid atitle gid gtitle new-eprange
                          lastup lastep completed?)))))))

(define (max-by length items default)
  (if (null? items)
      default
      (fold (lambda (item max)
              (if (> (length item) (length max)) item max))
            (car items) (cdr items))))

(define (group-with-newer-episodes eprange groups)
  (let ((old-episodes (expand-eprange eprange)))
    (max-by (lambda (group)
              (let* ((new-eprange (sxml:value ((sxpath "eprange") group)))
                     (new-episodes (expand-eprange new-eprange)))
                (length (lset-difference = new-episodes old-episodes))))
            groups #f)))

(define (check-for-any-release! db xml aid)
  (let* ((latest-data (latest-data-for-anime db aid))
         (old-eprange (alist-ref 'eprange latest-data))
         (completed? (alist-ref 'completed latest-data)))
    (when (not completed?)
      (let* ((atitle ((sxpath "string(//title[@type='main'])") xml))
             (epcount ((sxpath "number(//anime/neps)") xml))
             (groups ((sxpath "//group[sub/lang[text()='en']]") xml))
             (group (group-with-newer-episodes old-eprange groups))
             (new-eprange (sxml:value ((sxpath "eprange") group)))
             (gid (sxml:value ((sxpath "@id") group) string->number))
             (gtitle (sxml:value ((sxpath "name") group)))
             (lastup (sxml:value ((sxpath "lastup") group)))
             (lastep (sxml:value ((sxpath "lastep") group) string->number)))
        (when new-eprange
          (let ((completed? (equal? new-eprange (compact-eprange epcount))))
            (add-release! db aid atitle gid gtitle new-eprange
                          lastup lastep completed?)))))))

(define (check-for-releases! db path)
  (for-each
   (match-lambda
    ((aid . gid)
     (check-for-group-release! db (anime-xml aid) aid gid)
     (thread-sleep! 5))
    (aid
     (check-for-any-release! db (anime-xml aid) aid)
     (thread-sleep! 5)))
   (call-with-input-file path read)))

(define atom-limit 15)

(define (updated-at db)
  (query fetch-value (sql db "SELECT lastup FROM releases
                               ORDER BY lastup DESC LIMIT 1")))

(define (latest-posts db limit)
  (query fetch-all (sql db "SELECT aid, atitle, gid, gtitle,
                                   lastep, lastup, completed
                              FROM releases
                             ORDER BY lastup DESC LIMIT ?") limit))

(define (datestring->datetime string)
  (rfc3339->string (time->rfc3339 (string->time string "%Y-%m-%dT%H:%M:%S"))))

(define (permalink aid)
  (str "http://anidb.net/anime/" (number->string aid)))

(define (save-atom-feed! db path)
  (call-with-output-file path
    (lambda (out)
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
                    ((aid atitle gid gtitle lastep lastup completed)
                     (let* ((completed (sqlite-unbool completed))
                            (title (format "~a #~a (~a)" atitle lastep gtitle))
                            (title (str title (if completed " [END]" ""))))
                       (make-entry
                        id: (format "anidb:~a:~a:~a" aid gid lastep)
                        title: (make-title title)
                        updated: lastup
                        links: (list (make-link uri: (permalink aid)))))))
                   (latest-posts db atom-limit))))
       out))))

(define (main db-path aids-path feed-path)
  (call-with-database db-path
    (lambda (db)
      (check-for-releases! db aids-path)
      (save-atom-feed! db feed-path))))

(apply main (command-line-arguments))
