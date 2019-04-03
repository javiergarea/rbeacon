-module(prop_rbeacon_stateful).
-include_lib("proper/include/proper.hrl").

-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).
-export([close/1, publish/2, subscribe/2, unsubscribe/1]).

-record(test_state,{rbeacon=null,
                    message=null,
                    filter=[]}).

%% @doc Default property
prop_prop_rbeacon_stateful() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {H, S, Res} = run_commands(?MODULE, Cmds),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [H, S, Res]),
                          aggregate(command_names(Cmds), Res =:= ok))
            end).

%% @doc Returns the state in which each test case starts.
initial_state() ->
    #test_state{}.

%% @doc Command generator, S is the current state
command(S) ->
    oneof([{call, rbeacon, new, [user_udp_port()]},
           {call, ?MODULE, publish, [S#test_state.rbeacon, binary()]},
           {call, ?MODULE, subscribe, [S#test_state.rbeacon, binary()]},
           {call, ?MODULE, unsubscribe, [S#test_state.rbeacon]},
           {call, ?MODULE, close, [S#test_state.rbeacon]}
          ]).

% UDP ports 49152 through 65535
user_udp_port() ->
    integer(49152, 65535).

%% @doc Next state transformation, S is the current state. Returns next state.
next_state(S, V, {call, rbeacon, new, _Port}) ->
    S#test_state{rbeacon = V};
next_state(S, _V, {call, ?MODULE, publish, [_Beacon, Binary]}) ->
    S#test_state{message = Binary};
next_state(S, _V, {call, ?MODULE, subscribe, [_Beacon, Filter]}) ->
    S#test_state{filter = S#test_state.filter ++ [Filter]};
next_state(S, _V, {call, ?MODULE, unsubscribe, _Beacon}) ->
    S#test_state{filter = []};
next_state(S, _V, {call, ?MODULE, close, _Beacon}) ->
    S#test_state{rbeacon = null}.


%% @doc Precondition, checked before command is added to the command sequence.
precondition(S, {call, rbeacon, new, _Port}) ->
    S#test_state.rbeacon == null;
precondition(S, {call, ?MODULE, close, _Beacon}) ->
    S#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, publish, [_Beacon, _Binary]}) ->
    S#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, subscribe, [_Beacon, _Filter]}) ->
    S#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, unsubscribe, _Beacon}) ->
    S#test_state.rbeacon =/= null
    andalso S#test_state.filter =/= [];
precondition(_S, _Call) ->
    false.


%% @doc Postcondition, checked after command has been evaluated
%%      Note: S is the state before next_state(S, _, Call)
postcondition(_S, {call, rbeacon, new, _Port}, {ok, Beacon}) ->
    is_pid(Beacon);
postcondition(_S, {call, ?MODULE, publish, [_Beacon, _Binary]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, subscribe, [_Beacon, _Filter]}, ok) ->
    true;
postcondition(_S, {call, ?MODULE, unsubscribe, _Beacon}, ok) ->
    true;
postcondition(_S, _Call, _Res) ->
    true.

%% Wrappers para el modelo
close({ok, Beacon}) ->
    rbeacon:close(Beacon).

publish({ok, Beacon}, Binary) ->
    rbeacon:publish(Beacon, Binary).

subscribe({ok, Beacon}, Filter) ->
    rbeacon:subscribe(Beacon, Filter).

unsubscribe({ok, Beacon}) ->
    rbeacon:unsubscribe(Beacon).
