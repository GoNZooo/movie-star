#lang racket/base

(require web-server/servlet
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

(define (make-slack-payload message)
  (jsexpr->string `#hasheq((text . ,message))))

(define/page (actor-response actor)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (get-filmography actor
                            #:type "Actor")
           filmography->chatlist
           (string-append actor "\n" <>)
           make-slack-payload
           string->bytes/utf-8))))

(define/page (slack-actor-response actor)
  (define filmography (get-filmography actor
                                       #:type "Actor"))

  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (get-filmography actor
                            #:type "Actor")
           filmography->chatlist
           (string-append actor "\n" <>)
           make-slack-payload
           string->bytes/utf-8))))

(define/page (slack-cached-actor-response actor)
  (response/full
    200 #"Okay"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    `(,(~> (get-filmography/cached actor
                                   #:type "Actor")
           filmography->chatlist
           (string-append actor "\n" <>)
           string->bytes/utf-8))))

(define (request/actor-filmography request actor)
  (actor-response request
                  actor))

(define (slack-request/actor-filmography/post request)
  (define actor (extract-binding/single 'text (request-bindings request)))

  (if (in-cache? actor "Actor")
    (slack-cached-actor-response request actor)
    (slack-actor-response request actor)))

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
    [("movie-star" "filmography" "actor" (string-arg))
     request/actor-filmography]
    [("movie-star" "slack" "filmography" "actor")
     #:method "post"
     slack-request/actor-filmography/post]
    [("movie-star" "slack" "movie")
     #:method "post"
     slack-request/movie/post]))

(serve/servlet github-page-dispatch
               #:port 8082
               #:listen-ip #f
               #:servlet-regexp #rx""
               #:command-line? #t
               #:extra-files-paths `("static")
               #:servlet-current-directory "./"
               )

