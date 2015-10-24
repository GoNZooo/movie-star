#lang racket/base

(require "host-info.rkt")

(provide make-person-payload)
(define (make-person-payload filmography)
  (define message-lines (string-split (filmography->chatlist filmography)
                                      "\n"))
  (define title (car message-lines))

  (jsexpr->string
    `#hash((text . " ")
           (attachments .
                        (#hash((text . ,(string-append
                                          (cadr message-lines)
                                          (string-join (cddr message-lines)
                                                       ", ")))
                               (fallback . ,(format "Filmography for ~a"
                                                    title))
                               (title . ,title)))))))

(define (filmography->chatlist filmography)
  (foldl (lambda (m output)
           (string-append output
                          "\n"
                          (format "~a (~a)"
                                  (movie-title m)
                                  (movie-year m))))
         ""
         filmography))

(provide make-movie-payload)
(define (make-movie-payload movie)
  (define movie-title (hash-ref movie
                                'Title))
  (define year (hash-ref movie
                         'Year))
  (define rating (hash-ref movie
                           'imdbRating))
  (define director (hash-ref movie
                             'Director))
  (define actors (hash-ref movie
                           'Actors))
  (define genre (hash-ref movie
                          'Genre))
  (define plot (hash-ref movie
                         'Plot))
  (define imdb-url (string-append "http://www.imdb.com/title/"
                                  (hash-ref movie
                                            'imdbID)))
  (define imdb-poster-url (hash-ref movie
                                    'Poster))

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
        (write-bytes (call/input-url (string->url (hash-ref movie
                                                            'Poster))
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

