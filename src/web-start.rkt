#lang racket/base

(require racket/string
         racket/port

         net/url

         web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/dispatch
         web-server/page
         web-server/http/bindings

         json

         "jeapostrophe/threading-arrow.rkt"
         
         "api/myapifilmsapi.rkt"
         "api/omdbapi.rkt"
         "api/movie.rkt"
         
         "api/gonz/with-matches.rkt")

(define (filmography->chatlist filmography)
  (foldl (lambda (m output)
           (string-append output
                          "\n"
                          (format "~a (~a)"
                                  (movie-title m)
                                  (movie-year m))))
         ""
         filmography))

(define (make-person-payload message)
  (define message-lines (string-split message "\n"))
  (define title (car message-lines))

  (jsexpr->string
    `#hash((text . " ")
           (attachments .
                        (#hash((text . ,(string-append
                                          (cadr message-lines)
                                          (string-join (cddr message-lines)
                                                       ", ")))
                               (fallback . ,(format "Filmography for ~a"
                                                    title))
                               (title . ,title)))))))

(define/page (slack-movie-response title)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (movie/title->description title)
           string->bytes/utf-8))))

(define (slack-request/movie/post request)
  (define title (extract-binding/single 'text (request-bindings request)))
  
  (slack-movie-response request title))

(define/page (slack-actor-hook-response actor)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (get-filmography actor
                            #:type "Actor")
           filmography->chatlist
           (string-append actor "\n" <>)
           make-person-payload
           string->bytes/utf-8))))

(define (slack-request/actor-hook/post request)
  (define actor (~> (request-bindings request)
                    (extract-binding/single 'text <>)
                    (string-split <> "!actor ")
                    car))
  
  (slack-actor-hook-response request actor))

(define/page (slack-actress-hook-response actress)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (get-filmography actress
                            #:type "Actress")
           filmography->chatlist
           (string-append actress "\n" <>)
           make-person-payload
           string->bytes/utf-8))))

(define (slack-request/actress-hook/post request)
  (define actress (~> (request-bindings request)
                    (extract-binding/single 'text <>)
                    (string-split <> "!actress ")
                    car))
  
  (slack-actress-hook-response request actress))

(define/page (slack-director-hook-response director)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (get-filmography director
                            #:type "Director")
           filmography->chatlist
           (string-append director "\n" <>)
           make-person-payload
           string->bytes/utf-8))))

(define (slack-request/director-hook/post request)
  (define director (~> (request-bindings request)
                    (extract-binding/single 'text <>)
                    (string-split <> "!director ")
                    car))
  
  (slack-director-hook-response request director))

(define (make-movie-payload movie)
  (define movie-title (hash-ref movie 'Title))
  (define year (hash-ref movie 'Year))
  (define rating (hash-ref movie 'imdbRating))
  (define director (hash-ref movie 'Director))
  (define actors (hash-ref movie 'Actors))
  (define genre (hash-ref movie 'Genre))
  (define plot (hash-ref movie 'Plot))
  (define imdb-url (string-append "http://www.imdb.com/title/"
                                  (hash-ref movie 'imdbID)))
  (define imdb-poster-url (hash-ref movie 'Poster))
  (define local-poster-path
    (string-append "/movie-star/posters/"
                   (with-matches #px"/(\\w)/(.*)$"
                                 imdb-poster-url
                                 (string-append (m 1)
                                                (m 2)))))
  (define poster-url (string-append "http://severnatazvezda.com"
                                    local-poster-path))
  (when (not (file-exists? local-poster-path))
    (call-with-output-file
      local-poster-path
      (lambda (out)
        (write-bytes (call/input-url (string->url (hash-ref movie
                                                            'Poster))
                                     get-pure-port
                                     port->bytes)
                     out))
      #:mode 'binary))

  (jsexpr->string
    `#hash((text . " ")
           (attachments . (#hash((fields .
                                         (#hash((title . "Year")
                                                (value . ,year)
                                                (short . #t))
                                          #hash((title . "Rating")
                                                (value . ,rating)
                                                (short . #t))
                                          #hash((title . "Director")
                                                (value . ,director)
                                                (short . #t))
                                          #hash((title . "Actors")
                                                (value . ,actors)
                                                (short . #t))
                                          #hash((title . "Genre(s)")
                                                (value . ,genre)
                                                (short . #t))
                                          #hash((title . "Plot")
                                                (value . ,plot)
                                                (short . #t))))
                                 (fallback . ,(format "Movie info for '~a'"
                                                      movie-title))
                                 (title . ,movie-title)
                                 (title_link . ,imdb-url)
                                 (image_url . ,poster-url)))))))

(define/page (slack-movie-hook-response movie)
  (define js (movie/title->json movie))

  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (make-movie-payload js)
           string->bytes/utf-8))))

(define (slack-request/movie-hook/post request)
  (define movie (~> (request-bindings request)
                    (extract-binding/single 'text <>)
                    (string-split <> "!movie ")
                    car))
  
  (slack-movie-hook-response request movie))

(define/page (ping-page)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(string->bytes/utf-8 "Pong!"))))

(define (request/ping request)
  (ping-page request))

(define-values (github-page-dispatch github-page-url)
  (dispatch-rules
    [("ping") request/ping]
    [("movie-star" "slack" "movie")
     #:method "post"
     slack-request/movie/post]
    [("movie-star" "slack" "actor-hook")
     #:method "post"
     slack-request/actor-hook/post]
    [("movie-star" "slack" "actress-hook")
     #:method "post"
     slack-request/actress-hook/post]
    [("movie-star" "slack" "director-hook")
     #:method "post"
     slack-request/director-hook/post]
    [("movie-star" "slack" "movie-hook")
     #:method "post"
     slack-request/movie-hook/post]))

(serve/servlet github-page-dispatch
               #:port 8082
               #:listen-ip #f
               #:servlet-regexp #rx""
               #:command-line? #t
               #:extra-files-paths `("static")
               #:servlet-current-directory "./"
               )

