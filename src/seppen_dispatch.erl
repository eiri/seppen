-module(seppen_dispatch).

-export([start_link/0, init/1, all_shards/0, shards/1]).

-record(shard, {node, name, from, to}).

-define(REFRESH_TIME, timer:minutes(1)).

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

all_shards() ->
    Head = #shard{node = '$1', _ = '_'},
    ets:select(?MODULE, [{Head, [], ['$1']}]).

shards(<<N:8, _/binary>>) ->
    shards(N);
shards(N) when is_integer(N) ->
    Head = #shard{from = '$1', to = '$2', node = '$3', _ = '_'},
    Guards = [{'=<', '$1', N}, {'>=', '$2', N}],
    ets:select(?MODULE, [{Head, Guards, ['$3']}]).

init(Parent) ->
    register(?MODULE, self()),
    ets:new(?MODULE, [
        bag,
        named_table,
        protected,
        {read_concurrency, true},
        {keypos, #shard.node}
    ]),
    net_kernel:monitor_nodes(true),
    {ok, _} = timer:send_after(0, build_map),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent).

loop(Parent) ->
    receive
        build_map ->
            build_map(),
            {ok, _} = timer:send_after(?REFRESH_TIME, build_map),
            loop(Parent);
        {nodeup, Node} ->
            Ranges = get_ranges(Node),
            ets:insert(?MODULE, Ranges),
            loop(Parent);
        {nodedown, Node} ->
            ets:delete(?MODULE, Node),
            loop(Parent);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], [])
    end.

hosts() ->
    case persistent_term:get(seppen_hosts, undefined) of
        undefined ->
            Hosts = hosts(net_adm:host_file()),
            persistent_term:put(seppen_hosts, Hosts),
            Hosts;
        Hosts ->
            Hosts
    end.

hosts({error, _}) ->
    {ok, Host} = inet:gethostname(),
    ['127.0.0.1', list_to_atom(Host)];
hosts(FromHostFile) ->
    FromHostFile.

build_map() ->
    Hosts = hosts(),
    Nodes = net_adm:world_list(Hosts),
    lists:foreach(
        fun(Node) ->
            case ets:member(?MODULE, Node) of
                false ->
                    Ranges = get_ranges(Node),
                    ets:insert(?MODULE, Ranges);
                true ->
                    ok
            end
        end,
        Nodes
    ).

get_ranges(Node) ->
    case re:split(atom_to_list(Node), "[-@]", [{return, list}]) of
        [Name, From0, To0 | _] ->
            From = list_to_integer(From0),
            To = list_to_integer(To0),
            case From > To of
                true ->
                    [
                        #shard{node = Node, name = Name, from = 0, to = To},
                        #shard{node = Node, name = Name, from = From, to = 255}
                    ];
                false ->
                    [
                        #shard{node = Node, name = Name, from = From, to = To}
                    ]
            end;
        _ ->
            []
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

shards_test_() ->
    {foreach,
        fun() ->
            ets:new(?MODULE, [bag, named_table, public, {keypos, #shard.node}])
        end,
        fun(_) ->
            ets:delete(?MODULE)
        end,
        [
            {"all shards", fun test_all_shards/0},
            {"all avail, 1 shard, 1 copy", fun test_shards_1_1/0},
            {"all avail, 1 shard, 3 copies", fun test_shards_1_3/0},
            {"all avail, 3 shard, 1 copy", fun test_shards_3_1/0},
            {"all avail, 3 shard, 2 copy", fun test_shards_3_2/0},
            {"1 missing, 3 shard, 1 copy", fun test_shards_3_1_nodedown/0},
            {"1 missing, 3 shard, 2 copy", fun test_shards_3_2_nodedown/0}
        ]}.

test_all_shards() ->
    NodeA = 'a-0-255',
    NodeB = 'b-0-255',
    NodeC = 'c-0-255',
    Ranges = lists:append([
        get_ranges(NodeA),
        get_ranges(NodeB),
        get_ranges(NodeC)
    ]),
    ets:insert(?MODULE, Ranges),
    ?assertEqual([NodeA, NodeB, NodeC], lists:sort(all_shards())).

test_shards_1_1() ->
    Node = 'a-0-255',
    Ranges = get_ranges(Node),
    ets:insert(?MODULE, Ranges),
    ?assertEqual([Node], shards(0)),
    ?assertEqual([Node], shards(127)),
    ?assertEqual([Node], shards(255)).

test_shards_1_3() ->
    NodeA = 'a-0-255',
    NodeB = 'b-0-255',
    NodeC = 'c-0-255',
    Ranges = lists:append([
        get_ranges(NodeA),
        get_ranges(NodeB),
        get_ranges(NodeC)
    ]),
    ets:insert(?MODULE, Ranges),
    %% all three shards
    ?assertEqual([NodeA, NodeB, NodeC], lists:sort(shards(0))),
    ?assertEqual([NodeA, NodeB, NodeC], lists:sort(shards(127))),
    ?assertEqual([NodeA, NodeB, NodeC], lists:sort(shards(255))).

test_shards_3_1() ->
    NodeA = 'a-0-84',
    NodeB = 'b-85-169',
    NodeC = 'c-170-255',
    Ranges = lists:append([
        get_ranges(NodeA),
        get_ranges(NodeB),
        get_ranges(NodeC)
    ]),
    ets:insert(?MODULE, Ranges),
    %% first shard
    ?assertEqual([NodeA], shards(0)),
    ?assertEqual([NodeA], shards(42)),
    ?assertEqual([NodeA], shards(84)),
    %% second shard
    ?assertEqual([NodeB], shards(85)),
    ?assertEqual([NodeB], shards(127)),
    ?assertEqual([NodeB], shards(169)),
    %% third shard
    ?assertEqual([NodeC], shards(170)),
    ?assertEqual([NodeC], shards(212)),
    ?assertEqual([NodeC], shards(255)).

test_shards_3_2() ->
    NodeA = 'a-0-169',
    NodeB = 'b-85-255',
    NodeC = 'c-170-84',
    Ranges = lists:append([
        get_ranges(NodeA),
        get_ranges(NodeB),
        get_ranges(NodeC)
    ]),
    ets:insert(?MODULE, Ranges),
    %% shards 1 - 3
    ?assertEqual([NodeA, NodeC], lists:sort(shards(0))),
    ?assertEqual([NodeA, NodeC], lists:sort(shards(42))),
    ?assertEqual([NodeA, NodeC], lists:sort(shards(84))),
    %% shards 1 - 2
    ?assertEqual([NodeA, NodeB], lists:sort(shards(85))),
    ?assertEqual([NodeA, NodeB], lists:sort(shards(127))),
    ?assertEqual([NodeA, NodeB], lists:sort(shards(169))),
    %% shards 2 - 3
    ?assertEqual([NodeB, NodeC], lists:sort(shards(170))),
    ?assertEqual([NodeB, NodeC], lists:sort(shards(212))),
    ?assertEqual([NodeB, NodeC], lists:sort(shards(255))).

test_shards_3_1_nodedown() ->
    NodeA = 'a-0-84',
    NodeC = 'c-170-255',
    Ranges = lists:append([
        get_ranges(NodeA),
        get_ranges(NodeC)
    ]),
    ets:insert(?MODULE, Ranges),
    %% first shard
    ?assertEqual([NodeA], shards(0)),
    ?assertEqual([NodeA], shards(42)),
    ?assertEqual([NodeA], shards(84)),
    %% second shard
    ?assertEqual([], shards(85)),
    ?assertEqual([], shards(127)),
    ?assertEqual([], shards(169)),
    %% third shard
    ?assertEqual([NodeC], shards(170)),
    ?assertEqual([NodeC], shards(212)),
    ?assertEqual([NodeC], shards(255)).

test_shards_3_2_nodedown() ->
    NodeA = 'a-0-169',
    NodeC = 'c-170-84',
    Ranges = lists:append([
        get_ranges(NodeA),
        get_ranges(NodeC)
    ]),
    ets:insert(?MODULE, Ranges),
    %% shards 1 - 3
    ?assertEqual([NodeA, NodeC], lists:sort(shards(0))),
    ?assertEqual([NodeA, NodeC], lists:sort(shards(42))),
    ?assertEqual([NodeA, NodeC], lists:sort(shards(84))),
    %% shard 1
    ?assertEqual([NodeA], shards(85)),
    ?assertEqual([NodeA], shards(127)),
    ?assertEqual([NodeA], shards(169)),
    %% shard 3
    ?assertEqual([NodeC], shards(170)),
    ?assertEqual([NodeC], shards(212)),
    ?assertEqual([NodeC], shards(255)).

-endif.
