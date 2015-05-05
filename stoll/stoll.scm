(declare (uses quotes))

(use spiffy
     spiffy-uri-match
     (only posix set-buffering-mode!))

(define (random-quotes)
  (format "~A\n"
          (string-intersperse
           (sample (if (= (random 2) 0) 2 3)
                   quotes))))

(define (sample size population)
  (let ((population-size (vector-length population)))
    ;; take a sample by adding yet untaken picks to the result
    (let loop ((result '())
               (result-size 0))
      (if (< result-size size)
          (let ((pick (vector-ref population (random population-size))))
            (if (not (member pick result))
                (loop (cons pick result) (add1 result-size))
                (loop result result-size)))
          result))))

(define (main)
  (vhost-map
   `((".*" . ,(uri-match/spiffy
               `(((/ "")
                  (GET ,(lambda (c)
                          (send-response body: (random-quotes))))))))))
  (server-bind-address "127.0.0.1")
  (server-port 8000)
  (set-buffering-mode! (current-output-port) #:line)
  (access-log (current-output-port))
  (start-server))

(main)
