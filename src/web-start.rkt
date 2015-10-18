#lang racket/base

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/dispatch
         web-server/page

         json

         jeapostrophe/threading-arrow
         
         "api/myapifilmsapi.rkt"
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

(define/page (slack-actor-response actor)
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

(define (slack-request/actor-filmography request actor)
  (slack-actor-response request
                        actor))

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
    [("slack" "filmography" "actor" (string-arg)) slack-request/actor-filmography]))

(serve/servlet github-page-dispatch
               #:port 8082
               #:listen-ip #f
               #:servlet-regexp #rx""
               #:command-line? #t
               #:extra-files-paths `("static")
               #:servlet-current-directory "./"
               )

