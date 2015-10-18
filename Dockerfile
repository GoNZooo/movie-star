FROM gonz/racket
MAINTAINER Rickard Andersson <gonz@severnatazvezda.com>

# Copy movie-star source to filesystem
COPY src /movie-star-page-src
WORKDIR /movie-star-page-src

EXPOSE 8082
CMD ["racket", "web-start.rkt"]
