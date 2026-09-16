# Seppen (雪片)

[![Erlang CI](https://github.com/eiri/seppen/actions/workflows/erlang.yml/badge.svg)](https://github.com/eiri/seppen/actions/workflows/erlang.yml)

_Simple distributed in-memory key-value store_

## What is it

Yet another distributed in-memory key-value store with a REST interface built on Cowboy and ETS.

## Motivation

This is a pet project for studying how to deploy a distributed Erlang application in [Nomad](https://developer.hashicorp.com/nomad).

I'm toying with the idea of writing an Erlang [task driver](https://developer.hashicorp.com/nomad/docs/deploy/task-driver) for Nomad. I need software simple and familiar enough that its idiosyncrasies do not get in the way. This one-day project is meant to be exactly that.

## Architecture

### Storage

A gen_server owns a private ETS `set` table. Entries are indexed by the HMAC of their payload. The HMAC acts as the `uid`, so storing the same payload under different keys keeps only one copy. A separate index table maps keys to value HMACs. For simpler configuration, the Erlang cookie is the HMAC key.

### Sharding

#### Placement

Sharding uses the first byte of a value's HMAC, giving at most 256 shards. Each Erlang node holds an explicit range defined by its name: `{name}-{inclusive range start}-{inclusive range end}@hostname`. If the start is greater than the end, the range wraps through zero.

#### Configuration examples

Explicit shard ranges allow different numbers of shards and copies. For example:

- Single shard, single copy: `seppen-0-255@hostname`
- Three shards, single copy: `seppen-0-84@hostname`, `seppen-85-169@hostname`, `seppen-170-255@hostname`
- Three shards, two copies: `seppen-0-169@hostname`, `seppen-85-255@hostname`, `seppen-170-84@hostname`

#### Limitation

Seppen is a toy project built to study Erlang deployments in Nomad. It does not implement shard handoffs, persistent recovery, or split-brain resolution. These may be tackled later while looking at release upgrades and Nomad cluster degradation.

### Interface

The interface is a cookie-cutter [Cowboy](https://github.com/ninenines/cowboy) REST server. Each node's server is stateless; an external proxy such as HAProxy or Traefik handles load balancing.

## API

[OpenAPI 3.1 specification](openapi.yaml)

## Build and test

The examples use Erlang/OTP 29.0.6 and Rebar3 3.27.0. Erlang/OTP 27 or newer is supported.

```
$ make eunit ct
rebar3 eunit
===> Verifying dependencies...
===> Compiling seppen
===> Performing EUnit tests...
.................
17 tests, 0 failures
rebar3 ct --name seppen-0-255@127.0.0.1 --setcookie snowflake --suite seppen_SUITE
===> Verifying dependencies...
===> Compiling seppen
===> Running Common Test suites...
%%% seppen_SUITE: .........................
All 25 tests passed.
```

Distributed testing:

```
$ make dist
rebar3 ct --name ct_master@127.0.0.1 --setcookie snowflake --suite seppen_dist_SUITE --readable true -v
===> Verifying dependencies...
===> Compiling seppen
===> Running Common Test suites...
%%% seppen_dist_SUITE: .........
All 9 tests passed.
```

The distributed suite starts `ct1-0-127@127.0.0.1` and `ct2-128-255@127.0.0.1`. Each node holds one range, and the suite checks that data is distributed between them. If an interrupted run leaves test nodes behind, remove them with `make dist_clean`.

## Deploy to Nomad

_TBD_

## Name

Seppen (雪片) means "snowflake" in Japanese. It can also be written as 切片, meaning "segment". The name hints at the store's ephemeral data and sharded architecture.

## License

[MIT](LICENSE)
