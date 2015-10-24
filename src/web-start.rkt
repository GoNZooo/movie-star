#lang racket/base

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/dispatch
         web-server/page
         web-server/http/bindings

         "jeapostrophe/threading-arrow.rkt"
         
         "api/myapifilmsapi.rkt"
         "api/omdbapi.rkt"
         "api/slack.rkt"
         "api/movie.rkt"
         
         "api/gonz/with-matches.rkt")

(define/page (slack-actor-hook-response actor)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (get-filmography actor
                            #:type "Actor")
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
           make-person-payload
           string->bytes/utf-8))))

(define (slack-request/director-hook/post request)
  (define director (~> (request-bindings request)
                    (extract-binding/single 'text <>)
                    (string-split <> "!director ")
                    car))
  
  (slack-director-hook-response request director))

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

