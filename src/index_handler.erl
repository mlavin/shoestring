-module(index_handler).

-behavior(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-record(state, {room=undefined}).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Websocket API
%% ===================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    State = #state{room=undefined},
    {ok, Req, State}.
 
websocket_handle({text, <<"CREATE">>}, Req, State) ->
    Room = new_room_name(),
    ok = pg2:create(Room),
    ok = pg2:join(Room, self()),
    NewState = State#state{room=Room},
    {reply, {text, <<"CREATED ", Room/binary>>}, Req, NewState};
websocket_handle({text, <<"JOIN ", Room/binary>>}, Req, State) ->
    case pg2:get_members(Room) of
        {error, _} ->
            {reply, {text, <<"INVALID ROOM ", Room/binary>>}, Req, State};
        Members ->
            case length(Members) of
                1 ->
                    ok = pg2:join(Room, self()),
                    NewState = State#state{room=Room},
                    relay_message(<<"PEER CONNECTED">>, Room),
                    {reply, {text, <<"JOINED ", Room/binary>>}, Req, NewState};
                _ ->
                    {reply, {text, <<"INVALID ROOM ", Room/binary>>}, Req, State}
            end
    end;
websocket_handle({text, Data}, Req, State) ->
    case State#state.room of
        undefined ->
            {ok, Req, State};
        _ ->
            relay_message(Data, State#state.room),
            {ok, Req, State}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Info, Req, State) ->
    {reply, {text, Info}, Req, State}.
 
websocket_terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

new_room_name() ->
    Bin = crypto:rand_uniform(1, 100000),
    integer_to_binary(Bin).

relay_message(Msg, Room) ->
    [Pid ! <<Msg/binary>> || Pid <- pg2:get_members(Room) -- [self()]].


%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

new_room_name_type_test() ->
    Name = new_room_name(),
    ?assert(is_binary(Name)).

new_room_name_unique_test() ->
    Name = new_room_name(),
    Other = new_room_name(),
    ?assertNotEqual(Name, Other).

-endif.