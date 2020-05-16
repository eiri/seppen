# Seppen (雪片)
![common tests](https://github.com/eiri/seppen/workflows/Erlang%20CI/badge.svg?branch=master&event=push)

_Simple distributed in-memory key-value store_

## What is it

Yet another distributed in-memory key value store with REST interface build on top of cowboy and ets.

## Motivation

This is essentially a pet project built to study a best way to deploy a distributed erlang application in [Nomad](https://www.hashicorp.com/products/nomad).

I'm toying with an idea of writing Nomad's erlang [task driver](https://www.nomadproject.io/docs/drivers/) and need some piece of software simple and familiar enough that its idiosyncrasies wouldn't stand in a way. This one day projet should be exactly this

## Architecture

### Storage

Storage is gen_server owning a private ets `set` table. Entries stored indexed by hmac of payload which acts as `uid`, meaning that the same payload with different keys stored only once. Keys, pointing on values' hmacs, stored in a separate index table. To simplify config erlang cookie used as a key for hmac.

### Sharding

#### Placement

Sharding done with consistent hasing where each erlang node holds a shard range. The placement decided by first byte of value's hmac, so there might be max 256 shards. Ranges defined explicitly by nodes names provided in format `{name}-{range beginning inclusive}-{range ending inclusive}@hostname`. If beginning of the range large than ending, the node will hold two ranges flipped over 0.

#### Configuration examples

The explicit definition of a shard allows flexible configuration for number of shards and copies. For example:

- Single shard, single copy: `seppen-0-255@hostname`
- Three shards, single copy: `seppen-0-84@hostname`, `seppen-85-169@hostname`, `seppen-170-255@hostname`
- Three shards, two copies: `seppen-0-169@hostname`, `seppen-85-255@hostname`, `seppen-170-84@hostname`

#### Limitation

Seppend been a toy project done mostly to study deployment of erlang apps in Nomad doesn't deal with nodes failures, handovers, brain splits and the rest of the complex distributed data stuff. This maybe get tackled later, during looking at handeling relup and Nomad cluster degradation.

### Interface

Interface is a cookie-cutter [cowboy](https://github.com/ninenines/cowboy) REST web-server. The web server on each node is stateless, load-balancing done by external means (haproxy or traefik)

## API

[OpenAPI specs](https://github.com/eiri/seppen/blob/master/seppen-swagger.yaml)


## Build and test

```
$ make test
rebar3 eunit
===> Verifying dependencies...
===> Compiling seppen
===> Performing EUnit tests...
.......
Finished in 0.160 seconds
7 tests, 0 failures
rebar3 ct --name seppen-0-255@127.0.0.1 --setcookie legNe7mIRCiMEy5X --suite seppen_SUITE
===> Verifying dependencies...
===> Compiling seppen
===> Running Common Test suites...
%%% seppen_SUITE: ..................
All 18 tests passed.
```

Distribute testing:
```
$ make dist
rebar3 ct --name ct_master@127.0.0.1 --setcookie legNe7mIRCiMEy5X --suite seppen_dist_SUITE --readable true -v
===> Verifying dependencies...
===> Compiling seppen
===> Running Common Test suites...
...

TEST INFO: 1 test(s), 6 case(s) in 1 suite(s)

Testing lib.seppen.seppen_dist_SUITE: Starting test, 6 test cases
%%% seppen_dist_SUITE ==> test_put: OK
%%% seppen_dist_SUITE ==> test_get: OK
%%% seppen_dist_SUITE ==> test_get_if_none_match: OK
%%% seppen_dist_SUITE ==> test_get_list: OK
%%% seppen_dist_SUITE ==> test_delete: OK
%%% seppen_dist_SUITE ==> test_get_empty_list: OK
Testing lib.seppen.seppen_dist_SUITE: TEST COMPLETE, 6 ok, 0 failed of 6 test cases
...
```

Distributed testing suite starts two nodes 'ct1-0-127@127.0.0.1', 'ct2-128-255@127.0.0.1' each holding a range and confirms proper distribution of test data across the nodes. Been common tests there are a chance for testing to leave behind slave nodes on test crash, this could be cleaned up by `make dist_clean`

## Deploy to Nomad

_TBD_

## Name

Seppen (雪片) means "snowflake" in Japanese and also can be written as 切片, meaning "segment". The name hints on ephemeral nature of in-memory store and its distributed "sharded" architecture.

## License

[MIT](https://github.com/eiri/seppen/blob/master/LICENSE)
