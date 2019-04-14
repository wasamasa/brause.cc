(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken port))
(import (chicken random))
(import (chicken string))
(import spiffy)
(import spiffy-uri-match)

(include "quotes.scm")

(define (random-quotes)
  (format "~A\n"
          (string-intersperse
           (sample (if (= (pseudo-random-integer 2) 0) 2 3)
                   quotes))))

(define (sample size population)
  (let ((population-size (vector-length population)))
    ;; take a sample by adding yet untaken picks to the result
    (let loop ((result '())
               (result-size 0))
      (if (< result-size size)
          (let ((pick (vector-ref population
                                  (pseudo-random-integer population-size))))
            (if (not (member pick result))
                (loop (cons pick result) (add1 result-size))
                (loop result result-size)))
          result))))

(define (main)
  (trusted-proxies '("127.0.0.1"))
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
