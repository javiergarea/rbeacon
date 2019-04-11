-module(prop_rbeacon_stateful).
-include_lib("proper/include/proper.hrl").

-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).
-export([close/1, publish/2, subscribe/2, unsubscribe/1]).
-export([mock_new/1, mock_publish/2]).

-record(test_state,{rbeacon=null,
                    message=null,
                    filter=null,
                    port=null}).

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
    {#test_state{}, #test_state{}}.

%% @doc Command generator, S is the current state
command(S) ->
    {A, M} = S,
    oneof([{call, rbeacon, new, [user_udp_port()]},
           {call, ?MODULE, publish, [A#test_state.rbeacon, binary()]},
           {call, ?MODULE, subscribe, [A#test_state.rbeacon, oneof([binary(), ascii_string()])]},
           {call, ?MODULE, unsubscribe, [A#test_state.rbeacon]},
           {call, ?MODULE, close, [A#test_state.rbeacon]},
           {call, ?MODULE, mock_new, A#test_state.port},
           {call, ?MODULE, mock_publish, [M#test_state.rbeacon, binary()]}
          ]).

%ASCII string generator
ascii_string() ->
     ?SUCHTHAT(S, string(), S == [X || X <- S, X =< 255]).

% UDP ports 49152 through 65535
user_udp_port() ->
    integer(49152, 65535).

%% @doc Next state transformation, S is the current state. Returns next state.
next_state({A, M}, V, {call, rbeacon, new, Port}) ->
    {A#test_state{rbeacon = V, port = Port}, M};
next_state({A, M}, _V, {call, ?MODULE, publish, [_Beacon, Binary]}) ->
    {A#test_state{message = Binary}, M};
next_state({A, M}, _V, {call, ?MODULE, subscribe, [_Beacon, Filter]}) when A#test_state.filter == null ->
    {A#test_state{filter = [Filter]}, M};
next_state({A, M}, _V, {call, ?MODULE, subscribe, [_Beacon, Filter]}) ->
    {A#test_state{filter = A#test_state.filter ++ [Filter]}, M};
next_state({A, M}, _V, {call, ?MODULE, unsubscribe, _Beacon}) ->
    {A#test_state{filter = null}, M};
next_state({A, M}, _V, {call, ?MODULE, close, _Beacon}) ->
    {A#test_state{rbeacon = null}, M};
next_state({A, M}, V, {call, ?MODULE, mock_new, Port}) ->
    {A, M#test_state{rbeacon = V, port = Port}};
next_state({A, M}, _V, {call, ?MODULE, mock_publish, [_Beacon, Binary]}) ->
    {A, M#test_state{message = Binary}}.


%% @doc Precondition, checked before command is added to the command sequence.
precondition({A, _M}, {call, rbeacon, new, _Port}) ->
    A#test_state.rbeacon == null;
precondition({A, _M}, {call, ?MODULE, close, _Beacon}) ->
    A#test_state.rbeacon =/= null;
precondition({A, _M}, {call, ?MODULE, publish, [_Beacon, _Binary]}) ->
    A#test_state.rbeacon =/= null;
precondition({A, _M}, {call, ?MODULE, subscribe, [_Beacon, _Filter]}) ->
    A#test_state.rbeacon =/= null;
precondition({A, _M}, {call, ?MODULE, unsubscribe, _Beacon}) ->
    A#test_state.rbeacon =/= null
    andalso A#test_state.filter =/= null;
precondition({A, _M}, {call, ?MODULE, mock_new, _Port}) ->
    A#test_state.rbeacon =/= null;
precondition({_A, M}, {call, ?MODULE, mock_publish, [_Beacon, _Binary]}) ->
    M#test_state.rbeacon =/= null;
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
postcondition(_S, {call, ?MODULE, mock_new, _Port}, {ok, Beacon}) ->
    is_pid(Beacon);
postcondition(_S, {call, ?MODULE, mock_publish, [_Beacon, _Binary]}, ok) ->
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

%% Mock
mock_new(Port) ->
    rbeacon:new(Port).

mock_publish({ok, Beacon}, Binary) ->
    rbeacon:publish(Beacon, Binary).
