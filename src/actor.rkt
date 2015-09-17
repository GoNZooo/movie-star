#lang racket/base

(require racket/match
         racket/pretty
         
         "movie.rkt"
         "myapifilmsapi.rkt")

(provide (struct-out actor))
(struct actor (name filmography)
        #:transparent)

(provide load-actor)
(define (load-actor filename)
  (match (call-with-input-file filename read)
    [`(actor ,name (filmography ,films ...))
      (actor name (map make-movie films))]))

(provide write-actor)
(define (write-actor name)
  (define filmography
    (map (lambda (m)
           (list 'movie
                 (movie-title m)
                 (movie-year m)))
         (get-filmography name)))
  
  (call-with-output-file
    (format "/home/gonz/code/racket/movie-star/src/data/actors/~a.rktd"
            (string-downcase (regexp-replace* " " name "-")))
    (lambda (op)
      (pretty-write `(actor ,name
                            ,(cons 'filmography
                                   filmography))
                    op))
    #:exists 'replace))

(provide download-filmography)
(define (download-filmography a [sleep-time 180])
  (for-each
    (lambda (m)
      (movie->file m)
      (sleep sleep-time))
    (actor-filmography a)))

(module+ main

  (write-actor "Al Pacino"))
