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

(provide download-filmography)
(define (download-filmography a [sleep-time 180])
  (for-each
    (lambda (m)
      (movie->file m)
      (sleep sleep-time))
    (actor-filmography a)))

(module+ main
  (require racket/pretty)

  (pretty-print (load-actor "data/actors/robert-de-niro.rktd")))
