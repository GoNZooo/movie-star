#lang racket/base

(require racket/match)

(provide (struct-out movie))
(struct movie (title year)
        #:transparent)

(provide make-movie)
(define (make-movie m)
  (match m
    [`(movie ,title ,year)
      (movie title year)]))
