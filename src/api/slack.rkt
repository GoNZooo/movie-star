#lang racket/base

(require racket/string
         racket/port
         json
         net/url

         "gonz/with-matches.rkt"
         "host-info.rkt"
         "movie.rkt")

(provide make-person-payload)
(define (make-person-payload filmography)
  (define title (car filmography))
  (define filmography-list (films->chatlist (cdr filmography)))

  (jsexpr->string
    `#hash((text . " ")
           (attachments .
                        (#hash((text . ,filmography-list)
                               (fallback . ,(format "Filmography for ~a"
                                                    title))
                               (title . ,title)))))))

(define (films->chatlist films)
  (string-join (map (lambda (m)
                      (format "~a (~a)"
                              (movie-title m)
                              (movie-year m)))
                    films)
               ", "))

(provide make-movie-payload)
(define (make-movie-payload movie)
  (define (json-ref key)
    (hash-ref movie key))

  (define movie-title (json-ref 'Title))
  (define year (json-ref 'Year))
  (define rating (json-ref 'imdbRating))
  (define director (json-ref 'Director))
  (define actors (json-ref 'Actors))
  (define genre (json-ref 'Genre))
  (define plot (json-ref 'Plot))
  (define imdb-url (string-append "http://www.imdb.com/title/"
                                  (json-ref 'imdbID)))
  (define imdb-poster-url (json-ref 'Poster))

  (define local-poster-path
    (string-append "/movie-star/posters/"
                   (with-matches #px"/(\\w)/(.*)$"
                                 imdb-poster-url
                                 (string-append (m 1)
                                                (m 2)))))

  (define poster-url (string-append host-base-url
                                    local-poster-path))

  (when (not (file-exists? local-poster-path))
    (call-with-output-file
      local-poster-path
      (lambda (out)
        (write-bytes (call/input-url (string->url (json-ref 'Poster))
                                     get-pure-port
                                     port->bytes)
                     out))
      #:mode 'binary))

  (jsexpr->string
    `#hash((text . " ")
           (attachments . (#hash((fields .
                                         (#hash((title . "Year")
                                                (value . ,year)
                                                (short . #t))
                                          #hash((title . "Rating")
                                                (value . ,rating)
                                                (short . #t))
                                          #hash((title . "Director")
                                                (value . ,director)
                                                (short . #t))
                                          #hash((title . "Actors")
                                                (value . ,actors)
                                                (short . #t))
                                          #hash((title . "Genre(s)")
                                                (value . ,genre)
                                                (short . #t))
                                          #hash((title . "Plot")
                                                (value . ,plot)
                                                (short . #t))))
                                 (fallback . ,(format "Movie info for '~a'"
                                                      movie-title))
                                 (title . ,movie-title)
                                 (title_link . ,imdb-url)
                                 (image_url . ,poster-url)))))))

