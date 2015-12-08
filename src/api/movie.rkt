#lang racket/base

(require racket/match

         "omdbapi.rkt")

(provide (struct-out movie))
(struct movie (title year)
  #:transparent)

(provide make-movie)
(define (make-movie m)
  (match m
    [`(movie ,title ,year)
     (movie title year)]))

(provide movie->file)
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
