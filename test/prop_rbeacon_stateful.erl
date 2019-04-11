-module(prop_rbeacon_stateful).
-include_lib("proper/include/proper.hrl").

-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).
-export([close/1, publish/2, subscribe/2, unsubscribe/1, mock_new/1]).

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
    array:new([{size,2}, {fixed,true}, {default, #test_state{}}]).

%% @doc Command generator, S is the current state
command(S) ->
    A = array:get(0,S),
    M = array:get(1,S),
    oneof([{call, rbeacon, new, [user_udp_port()]},
           {call, ?MODULE, publish, [A#test_state.rbeacon, binary()]},
           {call, ?MODULE, subscribe, [A#test_state.rbeacon, oneof([binary(), ascii_string()])]},
           {call, ?MODULE, unsubscribe, [A#test_state.rbeacon]},
           {call, ?MODULE, close, [A#test_state.rbeacon]},
           {call, ?MODULE, mock_new, A#test_state.port}
          ]).

%ASCII string generator
ascii_string() ->
     ?SUCHTHAT(S, string(), S == [X || X <- S, X =< 255]).

% UDP ports 49152 through 65535
user_udp_port() ->
    integer(49152, 65535).

%% @doc Next state transformation, S is the current state. Returns next state.
next_state(S, V, {call, rbeacon, new, Port}) ->
    A = array:get(0,S),
    array:set(0, A#test_state{rbeacon = V, port = Port}, S);
next_state(S, _V, {call, ?MODULE, publish, [_Beacon, Binary]}) ->
    A = array:get(0,S),
    array:set(0, A#test_state{message = Binary}, S);
next_state(S, _V, {call, ?MODULE, subscribe, [_Beacon, Filter]}) ->
    A = array:get(0,S),
    case A#test_state.filter of 
        null -> array:set(0, A#test_state{filter = [Filter]}, S);
        _ -> array:set(0, A#test_state{filter = A#test_state.filter ++ [Filter]}, S)
    end;
next_state(S, _V, {call, ?MODULE, unsubscribe, _Beacon}) ->
    A = array:get(0,S),
    array:set(0, A#test_state{filter = null}, S);
next_state(S, _V, {call, ?MODULE, close, _Beacon}) ->
    A = array:get(0,S),
    array:set(0, A#test_state{rbeacon = null}, S);
next_state(S, V, {call, ?MODULE, mock_new, Port}) ->
    A = array:get(1,S),
    array:set(0, A#test_state{rbeacon = V, port = Port}, S).


%% @doc Precondition, checked before command is added to the command sequence.
precondition(S, {call, rbeacon, new, _Port}) ->
    A = array:get(0,S),
    A#test_state.rbeacon == null;
precondition(S, {call, ?MODULE, close, _Beacon}) ->
    A = array:get(0,S),
    A#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, publish, [_Beacon, _Binary]}) ->
    A = array:get(0,S),
    A#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, subscribe, [_Beacon, _Filter]}) ->
    A = array:get(0,S),
    A#test_state.rbeacon =/= null;
precondition(S, {call, ?MODULE, unsubscribe, _Beacon}) ->
    A = array:get(0,S),
    A#test_state.rbeacon =/= null
    andalso A#test_state.filter =/= null;
precondition(S, {call, ?MODULE, mock_new, _Port}) ->
    A = array:get(0,S),
    A#test_state.rbeacon =/= null;
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
