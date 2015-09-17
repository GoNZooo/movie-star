#lang racket/base

(require "../src/actor.rkt"
         "../src/movie.rkt")

(define robert-de-niro
  (load-actor
    "/home/gonz/code/racket/movie-star/src/data/actors/robert-de-niro.rktd"))

(module+ main
  (for-each
    (lambda (m)
      (movie->file m)
      (sleep 180))
    (actor-filmography robert-de-niro)))
