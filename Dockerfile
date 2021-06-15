FROM erlang:alpine AS builder

RUN apk add --no-cache git

RUN mkdir /build
WORKDIR /build

COPY . seppen

WORKDIR seppen
RUN rebar3 as prod release

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

COPY --from=builder /build/seppen/_build/prod/rel/seppen /seppen

EXPOSE 21285

CMD ["/seppen/bin/seppen", "foreground"]
