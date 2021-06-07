-module(seppen_hash).

-export([hmac/1, hmac/2, to_hex/1]).

hmac(Value) ->
    case persistent_term:get(seppen_key, undefined) of
        undefined ->
            Key = erlang:atom_to_binary(erlang:get_cookie(), unicode),
            persistent_term:put(seppen_key, Key),
            hmac(Key, Value);
        Key ->
            hmac(Key, Value)
    end.

hmac(Key, Value) ->
    crypto:mac(hmac, sha256, Key, Value).

to_hex(Bin) ->
    [
        begin
            if
                N < 10 -> 48 + N;
                true -> 87 + N
            end
        end
        || <<N:4>> <= Bin
    ].
