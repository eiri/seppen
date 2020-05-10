# Seppen (雪片)
Simple distributed in-memory key-value store

## What is it

Yet another distributed in-memory key value store with REST interface build on top of cowboy and ets.

## Motivation

This is essentially a pet project built to study a best way to deploy a distributed erlang application in [Nomad](https://www.hashicorp.com/products/nomad).

I'm toying with an idea of writing Nomad's erlang [task driver](https://www.nomadproject.io/docs/drivers/) and need some piece of software simple and familiar enough that its idiosyncrasies wouldn't stand in a way. This one day projet should be exactly this

## Architecture

### Storage

Storage is gen_server owning a private ets `set` table. Entries stored indexed by hmac of payload which acts as `uid`, meaning that the same payload with different keys stored only once. To simplify config erlang cookie used as a key for hmac.

### Sharding

Erlang's node name used to identify its bucket. On a write a node to which client got connected sends payload with `multicall` to all the nodes in cluster and hmac/uid of payload used to decide if it should be stored or reject on each particular node.

### Interface

Interface is a cookie-cutter [cowboy](https://github.com/ninenines/cowboy) REST web-server. The web server on each node is stateless, load-balancing done by external means (haproxy or traefik)

## API

```yaml
swagger: "2.0"
info:
  description: "Simple distributed in-memory key-value store"
  version: "1.0.0"
  title: "Seppen"
host: "localhost:21285"
basePath: "/"
schemes:
- "http"
paths:
  /:
    get:
      summary: "Returns list of the keys"
      produces:
      - "application/json"
      responses:
        200:
          description: "ok"
          schema:
            type: "array"
            items:
              type: "string"
  /{key}:
    parameters:
    - name: "key"
      in: "path"
      description: "key of the stored record"
      required: true
      type: "string"
    get:
      summary: "Return value for a given key"
      produces:
      - "application/json"
      responses:
        200:
          description: "ok"
          schema:
            type: "object"
        404:
          description: "not found"
    put:
      summary: "Store given payload under a given key"
      parameters:
      - in: "body"
        name: "value"
        required: true
        schema:
          type: "object"
      produces:
      - "application/json"
      responses:
        204:
          description: "no content"
        500:
          description: "internal server error"
    delete:
      summary: "Delete record for a given key"
      produces:
      - "application/json"
      responses:
        204:
          description: "no content"
        500:
          description: "internal server error"
```

## Build and test

_TBD_

## Deploy to Nomad

_TBD_

## Name

Seppen (雪片) means "snowflake" in Japanese and also can be written as 切片, meaning "segment". The name hints on ephemeral nature of in-memory store and its distributed "sharded" architecture.

## License

[MIT](https://github.com/eiri/seppen/blob/master/LICENSE)
