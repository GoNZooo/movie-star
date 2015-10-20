#lang racket/base

(require racket/match
         racket/port
         racket/cmdline
         net/url
         json

         "gonz/with-matches.rkt"
         
         "movie.rkt")

(define my-api-film-url
  "http://www.myapifilms.com/name?name=~a&filmography=1&format=JSON")

(define (person->jsexpr person #:type [type "Actor"] [cache? #t])
  (call/input-url (string->url (format my-api-film-url
                                       person))
                  get-pure-port
                  read-json))

(provide get-filmography)
(define (get-filmography person
                         #:type [type "Actor"]
                         #:cache? [cache? #t])
  (define (year->number y)
    ;; The API prepends a space for the year, for some reason. Using
    ;; `with-matches` to extract the year in a reliable way
    ;; Could just substring it out, but this might fail if the API
    ;; stops prepending the (weird unicode) space
    (if (not (< (string-length y)
                4))
      (with-matches
        #px"(\\d{4})" y
        (string->number (m 1)))
      -1))

  (if (and (in-cache? person type)
           cache?)
    (get-filmography/cached person #:type type)
    (let ([js-data (person->jsexpr person #:type type)])
      (write-cache person type js-data)

      (match
        (hash-ref (car js-data)
                  'filmographies)
        [(list fs ... a ofs ...)
         #:when (equal? (hash-ref a 'section)
                        type)
         (for/list ([m (hash-ref a 'filmography)])
           (movie (hash-ref m 'title)
                  (year->number (hash-ref m 'year))))]))))

(define (sanitize-name name)
  (regexp-replace #px"[ ']" (string-downcase name) ""))

(define (cache-path person type)
  (build-path 'up
              "cache"
              (format "~a_~a.cache"
                      type
                      (sanitize-name person))))

(provide in-cache?)
(define (in-cache? person type)
  (file-exists? (cache-path person type)))

(provide write-cache)
(define (write-cache person type filmography)
  (call-with-output-file (cache-path person type)
                         (lambda (output-port)
                           (write filmography output-port))
                         #:exists 'replace))

(provide get-filmography/cached)
(define (get-filmography/cached person #:type [type "Actor"])
  (define (year->number y)
    ;; The API prepends a space for the year, for some reason. Using
    ;; `with-matches` to extract the year in a reliable way
    ;; Could just substring it out, but this might fail if the API
    ;; stops prepending the (weird unicode) space
    (if (not (< (string-length y)
                4))
      (with-matches
        #px"(\\d{4})" y
        (string->number (m 1)))
      -1))

  (match
    (hash-ref (car (call-with-input-file
                     (cache-path person type)
                     read))
           'filmographies)
    [(list fs ... a ofs ...)
     #:when (equal? (hash-ref a 'section)
                    type)
     (for/list ([m (hash-ref a 'filmography)])
       (movie (hash-ref m 'title)
              (year->number (hash-ref m 'year))))]))

(define (cmdline-options)
  (define name (make-parameter ""))
  (define type (make-parameter "Actor"))

  (command-line
    #:once-each
    [("-t" "--type")
     arg-type
     "Set the type to search for (director, actor, etc.)"
     (type arg-type)]
    #:args (name)

    (values name (type))))

(module+ main
  (require racket/pretty)

  (define-values (name type)
    (cmdline-options))

  (pretty-print (get-filmography name #:type type)))
