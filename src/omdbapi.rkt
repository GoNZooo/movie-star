#lang racket/base

(require net/http-client
         json)

(define omdb-host "www.omdbapi.com")

(define title-year-url "?t=~a&y=~a&plot=full&r=json")
(define title-url "?t=~a&y=~a&plot=full&r=json")

(define (movie/title-year->json title year)
  (define-values (status headers data-port)
    (http-sendrecv omdb-host
                   (format title-year-url
                           title
                           year)))
  (read-json data-port))

(define (movie/title->json title)
  (define-values (status headers data-port)
    (http-sendrecv omdb-host
                   (format title-url
                           title)))
  (read-json data-port))
