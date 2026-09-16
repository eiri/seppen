FROM erlang:29.0.6-alpine@sha256:efc3f91ea52651bdb3582b137c968c063379134c1a0aa09ce2002205a2aee1d1 AS builder

WORKDIR /build
COPY . .
RUN rebar3 as prod release

FROM alpine:3.23.5@sha256:fd791d74b68913cbb027c6546007b3f0d3bc45125f797758156952bc2d6daf40

LABEL org.opencontainers.image.title="Seppen" \
      org.opencontainers.image.description="Distributed in-memory key-value store" \
      org.opencontainers.image.version="1.0.0" \
      org.opencontainers.image.source="https://github.com/eiri/seppen" \
      org.opencontainers.image.licenses="MIT"

RUN apk add --no-cache ca-certificates libstdc++ lksctp-tools ncurses-libs openssl && \
    addgroup -S -g 10001 seppen && \
    adduser -S -D -H -u 10001 -G seppen seppen

ENV HOME=/tmp \
    ERL_CRASH_DUMP=/tmp/erl_crash.dump \
    RELX_OUT_FILE_PATH=/tmp

WORKDIR /seppen
COPY --from=builder --chown=10001:10001 /build/_build/prod/rel/seppen/ ./

USER 10001:10001

EXPOSE 21285

HEALTHCHECK --interval=10s --timeout=3s --start-period=5s --retries=3 \
    CMD wget -q -O /dev/null http://127.0.0.1:21285/healthz || exit 1

STOPSIGNAL SIGTERM
CMD ["/seppen/bin/seppen", "foreground"]
