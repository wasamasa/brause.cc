(use (only http-client with-input-from-request)
     (only html-parser html->sxml)
     (only uri-common uri-reference uri-path uri->string update-uri)
     (only sxpath sxpath)
     (only matchable match-let*)
     (only sql-de-lite open-database fetch-value fetch-all
           sql schema exec query))

(define (fetch-fragment)
  (let* ((base-url "https://www.facebook.com/DiefetteKuh")
         (document (with-input-from-request base-url #f html->sxml))
         ;; NOTE: For yet unknown reasons Facebook keeps the
         ;; interesting data in a comment, that comment can be found
         ;; by picking the first containing "BURGER"
         (comment-string ((sxpath "string(//comment()[contains(.,'BURGER')])")
                          document))
         (comment (call-with-input-string comment-string html->sxml)))
    ;; Pick the first burger post
    (car ((sxpath "//div[contains(@class,'userContentWrapper') and contains(string(),'BURGER')]")
          comment))))

(define (burger-metadata fragment)
  (let* ((timestamp ((sxpath "number(//abbr/@data-utime)") fragment))
         (description ((sxpath "string(//div[contains(@class,'userContent')]/p)")
                       fragment))
         (image ((sxpath "string(//div[contains(@class,'uiScaledImageContainer')]/img/@src)")
                 fragment))
         (base-url "https://www.facebook.com")
         (relative-permalink ((sxpath "string(//a/abbr/../@href)") fragment))
         (path (uri-path (uri-reference relative-permalink)))
         (permalink (uri->string (update-uri (uri-reference base-url)
                                             path: path))))
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
  (update-database (burger-metadata (fetch-fragment))))

(main)
