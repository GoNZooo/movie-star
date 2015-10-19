#lang racket/base

(require racket/port
         racket/cmdline

         net/url
         json)

(define title-year-url "http://www.omdbapi.com/?t=~a&y=~a&plot=full&r=json")
(define title-url "http://www.omdbapi.com/?t=~a&plot=full&r=json")

(provide movie/title-year->json)
(define (movie/title-year->json title year)
  (call/input-url (string->url (format title-year-url
                                       title
                                       year))
                  get-pure-port
                  read-json))

(provide movie/title->json)
(define (movie/title->json title)
  (call/input-url (string->url (format title-url
                                       title))
                  get-pure-port
                  read-json))

(provide movie/title->description)
(define (movie/title->description title)
  (define js (movie/title->json title))
  (define movie-title (hash-ref js 'Title))
  (define year (hash-ref js 'Year))
  (define rating (hash-ref js 'imdbRating))
  (define director (hash-ref js 'Director))
  (define actors (hash-ref js 'Actors))
  (define genre (hash-ref js 'Genre))
  (define synopsis (hash-ref js 'Plot))
  (format "~a (~a) [~a] directed by ~a~n~a~n~a~n~a"
          movie-title year rating director actors genre synopsis))

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
