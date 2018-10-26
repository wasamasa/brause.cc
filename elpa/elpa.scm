(use (only data-structures substring-index)
     (only http-client with-input-from-request server-connector)
     (only uri-common uri-scheme uri-host uri-port)
     (only tcp tcp-connect)
     (only openssl ssl-connect*)
     (only matchable match match-let match-let* match-lambda)
     (only sql-de-lite open-database with-transaction
           schema exec sql query fetch-column fetch-value fetch-all)
     (only rfc3339 seconds->rfc3339 rfc3339->string)
     (only atom write-atom-doc make-atom-doc make-feed make-title
           make-author make-link make-entry make-content)
     sxml-serializer)

(define ssl-verify? (make-parameter #t))

;; adapted from http-client.scm
(define (http-server-connector uri proxy)
  (let ((remote-end (or proxy uri)))
    (case (uri-scheme remote-end)
      ((#f http) (tcp-connect (uri-host remote-end) (uri-port remote-end)))
      ((https) (receive (in out)
                   (ssl-connect* hostname: (uri-host remote-end)
                                 port: (uri-port remote-end)
                                 sni-name: #t
                                 verify?: (ssl-verify?))
                 (if (and in out)       ; Ugly, but necessary
                     (values in out)
                     (error "You forgot installing the openssl egg."))))
      (else (error "This shouldn't happen")))))

(server-connector http-server-connector)

(define elpa-meta-data
  '((gnu
     (title . "GNU ELPA")
     (archive-contents . "https://elpa.gnu.org/packages/archive-contents")
     (permalink . "https://elpa.gnu.org/packages/~a.html")
     (home-page . "https://elpa.gnu.org/"))
    (marmalade
     (title . "Marmalade")
     (archive-contents . "http://marmalade-repo.org/packages/archive-contents")
     (permalink . "https://marmalade-repo.org/packages/~a")
     (home-page . "https://marmalade-repo.org/")
     ;; expired certificate
     (disable-verification? . #t))
    (melpa
     (title . "MELPA")
     (archive-contents . "https://melpa.org/packages/archive-contents")
     (permalink . "https://melpa.org/#/~a")
     (home-page . "https://melpa.org/"))
    (melpa-stable
     (title . "MELPA Stable")
     (archive-contents . "https://stable.melpa.org/packages/archive-contents")
     (permalink . "https://stable.melpa.org/#/~a")
     (home-page . "https://stable.melpa.org/"))))

(define (archive-contents archive)
  (let* ((meta-data (alist-ref archive elpa-meta-data))
         (contents (alist-ref 'archive-contents meta-data)))
    (with-input-from-request contents #f read)))

(define (package-permalink package archive)
  (let* ((meta-data (alist-ref archive elpa-meta-data))
         (permalink (alist-ref 'permalink meta-data)))
    (format permalink package)))

(define (archive-home-page archive)
  (let ((meta-data (alist-ref archive elpa-meta-data)))
    (alist-ref 'home-page meta-data)))

(define (archive-title archive)
  (let ((meta-data (alist-ref archive elpa-meta-data)))
    (alist-ref 'title meta-data)))

(define (string-prefix? prefix string)
  (let ((index (substring-index prefix string)))
    (and index (zero? index))))

(define (fixup-url url)
  (if (or (string-prefix? "http://" url)
          (string-prefix? "https://" url))
      url
      (string-append "http://" url)))

(define (archive-item->list archive-item)
  ;; FIXME this will bite me once negative numbers for version parts
  ;; come into use...
  (define (concat seq sep)
    (string-intersperse (map ->string seq) sep))
  (match-let* (((name version _ desc _ . rest) archive-item)
               (meta-data (match rest
                            (((_ . _)) (car rest))
                            (else '())))
               (url (alist-ref ':url meta-data))
               (url (and url (fixup-url url)))
               (keywords (alist-ref ':keywords meta-data)))
    (list (symbol->string name) (concat version ".") desc url
          (and keywords (not (null? keywords))
               (concat keywords ", ")))))

(define (transform-archive-contents archive)
  (match-let (((_ . packages) (archive-contents archive)))
    (map archive-item->list packages)))

(define (init-database db)
  (if (null? (schema db))
      (exec (sql db "CREATE TABLE packages(id INTEGER PRIMARY KEY,
                                           archive TEXT,
                                           name TEXT,
                                           version TEXT,
                                           desc TEXT,
                                           url TEXT,
                                           keywords TEXT,
                                           added INTEGER,
                                           permalink TEXT)"))))

(define (fetch-known-packages db archive)
  (query fetch-column
         (sql db "SELECT name FROM packages WHERE archive = ?")
         (symbol->string archive)))

(define (insert-package! db archive name version desc url keywords added permalink)
  (exec (sql db "INSERT INTO packages(archive, name, version, desc, url, keywords, added, permalink)
                        VALUES(?, ?, ?, ?, ?, ?, ?, ?)")
        (symbol->string archive) name version desc
        (or url "") (or keywords "") added permalink))

(define (insert-packages! db archive)
  (let* ((timestamp (current-seconds))
         (known-packages (fetch-known-packages db archive))
         (archive-packages (transform-archive-contents archive))
         (unknown-packages (remove (lambda (package)
                                     (member (car package) known-packages))
                                   archive-packages)))
    (with-transaction db
      (lambda ()
        (for-each
         (match-lambda
          ((name version desc url keywords)
           (let ((permalink (package-permalink name archive)))
             (insert-package! db archive name version desc url
                              keywords timestamp permalink))))
         unknown-packages)))))

(define (updated-at db)
  (query fetch-value
         (sql db "SELECT added FROM packages ORDER BY added DESC LIMIT 1;")))

(define (latest-packages db archive limit)
  (query fetch-all
         (sql db "SELECT name, version, desc, url, keywords, added, permalink FROM packages WHERE archive = ? ORDER BY added DESC LIMIT ?")
         (symbol->string archive) limit))

(define (unix->datetime seconds)
  (rfc3339->string (seconds->rfc3339 seconds)))

(define (atom-feed-item name version desc url keywords)
  (serialize-sxml
   `(div
     "Name: " ,name (br)
     "Version: " ,version (br)
     "Description: " ,desc (br)
     "URL: " (a (@ (href ,url)) ,url) (br)
     "Keywords: " ,keywords (br))))

(define (atom-feed db archive file packages)
  (write-atom-doc
   (make-atom-doc
    (make-feed
     title: (make-title (format "~a Packages" (archive-title archive)))
     id: (format "http://elpa.brause.cc/~a" file)
     updated: (unix->datetime (updated-at db))
     authors: (list (make-author name: (symbol->string archive)))
     links: (list (make-link uri: (archive-home-page archive)))
     entries: (map
               (match-lambda
                ((name version desc url keywords added permalink)
                 (make-entry
                  id: (format "~a:~a" archive permalink)
                  title: (make-title name)
                  updated: (unix->datetime added)
                  links: (list (make-link uri: permalink))
                  content: (make-content
                            (atom-feed-item name version desc url keywords)
                            type: 'html))))
               packages)))))

(define atom-limit 10)

(define (die message #!rest args)
  (apply fprintf (current-error-port) message args)
  (exit 1))

(define (main archive outdir)
  (let ((archive (string->symbol archive))
        (db (open-database "db.sqlite3")))
    (init-database db)
    (when (not (alist-ref archive elpa-meta-data))
      (die "unknown archive: ~a\n" archive))
    (when (alist-ref 'disable-verification? (alist-ref archive elpa-meta-data))
      (ssl-verify? #f))
    (insert-packages! db archive)
    (let* ((packages (latest-packages db archive atom-limit))
           (file (format "~a.xml" archive)))
      (with-output-to-file (format "~a/~a" outdir file)
        (lambda () (atom-feed db archive file packages))))))

(when (not (= (length (command-line-arguments)) 2))
  (die "usage: ~a <archive-name> <outdir>\n" (program-name)))

(apply main (command-line-arguments))
