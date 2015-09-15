#lang racket/base

(require racket/port
         racket/cmdline
         net/url
         json)

(define title-year-url "http://www.omdbapi.com/?t=~a&y=~a&plot=full&r=json")
(define title-url "http://www.omdbapi.com/?t=~a&plot=full&r=json")

(define (movie/title-year->json title year)
  (call/input-url (string->url (format title-year-url
                                       title
                                       year))
                  get-pure-port
                  read-json))

(define (movie/title->json title)
  (call/input-url (string->url (format title-url
                                       title))
                  get-pure-port
                  read-json))

(define (cmdline-options)
  (define year (make-parameter -1))

  (command-line
    #:once-each
    [("-y" "--year")
     arg-year
     "Set the year of the movie to search for"
     (year (string->number arg-year))]
    #:args (title)

    (values title (year))))

(module+ main
  (define-values (title year)
    (cmdline-options))

  (if (= year -1)
    (movie/title->json title)
    (movie/title-year->json title year)))
