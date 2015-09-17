#lang racket/base

(require racket/match
         racket/port
         racket/cmdline
         net/url
         json

         gonz/with-matches
         
         "movie.rkt")

(define my-api-film-url
  "http://www.myapifilms.com/name?name=~a&filmography=1&format=JSON")

(provide get-filmography)
(define (get-filmography person [type "Actor"])
  (define (year->number y)
    ;; The API prepends a space for the year, for some reason. Using
    ;; `with-matches` to extract the year in a reliable way
    ;; Could just substring it out, but this might fail if the API
    ;; stops prepending the (weird unicode) space
    (with-matches
      #px"(\\d{4})" y
      (string->number (m 1))))

  (match (hash-ref (car (call/input-url (string->url (format my-api-film-url
                                                             person))
                                        get-pure-port
                                        read-json))
                   'filmographies)
    [(list fs ... a ofs ...)
     #:when (equal? (hash-ref a 'section)
                    type)
     (for/list ([m (hash-ref a 'filmography)])
       (movie (hash-ref m 'title)
              (year->number (hash-ref m 'year))))]))

(module+ main
  (require racket/pretty)

  (pretty-print (get-filmography "Al Pacino")))