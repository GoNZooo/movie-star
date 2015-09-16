#lang racket/base

(require racket/match
         
         "movie.rkt")

(provide (struct-out actor))
(struct actor (name filmography)
        #:transparent)

(provide load-actor)
(define (load-actor filename)
  (match (call-with-input-file filename read)
    [`(actor ,name (filmography ,films ...))
      (actor name (map make-movie films))]))

(module+ main
  (require racket/pretty)

  (pretty-print (load-actor "data/actors/robert-de-niro.rktd")))
