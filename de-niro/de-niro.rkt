#lang racket/base

(require "../src/actor.rkt"
         "../src/movie.rkt")

(define robert-de-niro
  (load-actor
    "/home/gonz/code/racket/movie-star/src/data/actors/robert-de-niro.rktd"))

(module+ main
  (download-filmography robert-de-niro))
