#lang racket/base

(require racket/string

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
         "api/movie.rkt")

(define (filmography->chatlist filmography)
  (foldl (lambda (m output)
           (string-append output
                          "\n"
                          (format "~a (~a)"
                                  (movie-title m)
                                  (movie-year m))))
         ""
         filmography))

(define (make-actor-payload message)
  (define message-lines (string-split message "\n"))
  (define title (car message-lines))

  (jsexpr->string
    `#hash((text . " ")
           (attachments . (#hash((text . ,(string-join (cdr message-lines)))
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
           make-actor-payload
           string->bytes/utf-8))))

(define (slack-request/actor-hook/post request)
  (define actor (~> (request-bindings request)
                    (extract-binding/single 'text <>)
                    (string-split <> "!actor ")
                    car))
  
  (slack-actor-hook-response request actor))

(define (make-movie-payload movie)
  (define movie-title (hash-ref movie 'Title))
  (define year (hash-ref movie 'Year))
  (define rating (hash-ref movie 'imdbRating))
  (define director (hash-ref movie 'Director))
  (define actors (hash-ref movie 'Actors))
  (define genre (hash-ref movie 'Genre))
  (define synopsis (hash-ref movie 'Plot))

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
                                          #hash((title . "Synopsis")
                                                (value . ,synopsis)
                                                (short . #f)))
                                         )
                                 (fallback . ,(format "Movie info for '~a'"
                                                      movie-title))
                                 (title . ,movie-title)))))))

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

