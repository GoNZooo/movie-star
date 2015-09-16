#lang racket/base

(require "../src/omdbapi.rkt"
         "../src/actor.rkt"
         "../src/movie.rkt")

(define robert-de-niro (load-actor "../src/data/actors/robert-de-niro.rktd"))

(define (movie->file m)
  (call-with-output-file
    (string-downcase (format "~a_~a.movie"
                             (movie-year m)
                             (regexp-replace* " "
                                              (movie-title m)
                                              "-")))
                     (lambda (op)
                       (write (movie/title-year->json (movie-title m)
                                                      (movie-year m))
                              op))
                     #:exists 'replace))

(module+ main
  (for-each
    (lambda (m)
      (movie->file m)
      (sleep 120))
    (actor-filmography robert-de-niro)))
