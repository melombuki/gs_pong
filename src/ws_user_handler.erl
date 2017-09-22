-module(ws_user_handler).

-export([init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-include("../include/gs_user.hrl").

-define(GAME_SERVICE, gs_game_service).
-define(GAME_ROOM, gs_game_room).
-define(GAME_OBJECT, gs_game_object).

-import(mochijson2, [encode/1, decode/1]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 600000}}.

websocket_init(State) ->
    {ok, State#user{pid = self()}, hibernate}.

websocket_handle({text, Msg}, State) ->
    {struct, JsonData} = decode(Msg),
    case proplists:get_value(<<"type">>, JsonData) of
        <<"select_username">> ->
            {ok, Name} = apply(?GAME_SERVICE, new_user, [State#user.name]),
            Resp = to_json_string({struct, [{<<"msg">>, Name}]}),
            {reply, {text, <<Resp/binary>>}, State};
        <<"chat_msg">> ->
            UserMsg = binary_to_list(proplists:get_value(<<"msg">>, JsonData)),
            Room = proplists:get_value(<<"room">>, JsonData),
            case apply(?GAME_SERVICE, get_room, [Room]) of
                {ok, RoomPid} ->
                    % TODO add the sending users name to the broadcast
                    apply(?GAME_ROOM, chat_broadcast, [RoomPid, UserMsg]),
                    {ok, State};
                no_such_room ->
                    Resp = to_json_string({struct, [{<<"msg">>, list_to_binary("Failed to send your message.")}]}),
                    {reply, {text, <<Resp/binary>>}, State}
            end;
        <<"join_chat_room">> ->
            Room = proplists:get_value(<<"room">>, JsonData),
            case apply(?GAME_SERVICE, get_room, [Room]) of
                {ok, RoomPid} ->
                    ok = apply(?GAME_ROOM, add, [RoomPid, State#user.pid]),
                    State1 = State#user{room_name = Room, room_pid = RoomPid},
                    Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(State#user.name)]))}]}),
                    {reply, {text, <<Resp/binary>>}, State1};
                no_such_room ->
                    case apply(?GAME_SERVICE, new_room, [Room]) of
                        {room_created, RoomPid} ->
                            State1 = State#user{room_name = Room, room_pid = RoomPid},
                            apply(?GAME_ROOM, add, [RoomPid, State#user.pid]),
                            Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(State#user.name)]))}]}),
                            {reply, {text, <<Resp/binary>>}, State1};
                        room_already_exists ->
                            Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["The room already exists. Try a different room name,  ", binary_to_list(State#user.name)]))}]}),
                            {reply, {text, <<Resp/binary>>}, State}
                    end
            end;
        <<"leave_chat_room">> ->
            ok = apply(?GAME_ROOM, remove, [State#user.room_pid, State#user.pid]),
            Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually droped you, ", binary_to_list(State#user.name)]))}]}),
            {reply, {text, <<Resp/binary>>}, State};
        <<"input">> ->
            Key = proplists:get_value(<<"key">>, JsonData),
            apply(?GAME_ROOM, handle_input, [State#user.room_pid, State#user.pid, Key]),
            {ok, State};
        <<"start_game">> ->
            Resp = case State#user.room_pid of
                RoomPid when State#user.room_pid =/= undefined ->
                    % TODO add the sending users name to the broadcast
                    apply(?GAME_ROOM, start_game, [RoomPid]),
                    to_json_string({struct, [{<<"msg">>, <<"Started your game">>}]});
                _ ->
                    to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["You are not in a game room, ", binary_to_list(State#user.name), ". Try a different room name."]))}]})
            end,
            {reply, {text, <<Resp/binary>>}, State};
        <<"stop_game">> ->
            Resp = case State#user.room_pid of
                RoomPid when State#user.room_pid =/= undefined ->
                    % TODO add the sending users name to the broadcast
                    apply(?GAME_ROOM, stop_game, [RoomPid]),
                    to_json_string({struct, [{<<"msg">>, <<"Started your game">>}]});
                _ ->
                    to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["You are not in a game room, ", binary_to_list(State#user.name), ". Try a different room name."]))}]})
            end,
            {reply, {text, <<Resp/binary>>}, State};
        _ ->
            Resp = to_json_string({struct, [{<<"msg">>, <<"I didn't quite get that.">>}]}),
            {reply, {text, <<Resp/binary>>}, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
    {ok, State, hibernate};
websocket_info({chat_broadcast, Msg}, State) ->
    {reply, {text, to_json_string({struct, [{<<"msg">>, list_to_binary(Msg)}]})}, State};
websocket_info({game_objects, Root}, State) ->
    GameObjects = apply(?GAME_OBJECT, to_proplist, [Root]),
    Resp = to_json_string({struct, [{<<"game_objects">>, GameObjects}]}),
    {reply, {text, <<Resp/binary>>}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _ConnState, State) ->
    io:format("~p - Terminated with reason: ~p~n", [self(), Reason]),
    gs_riak_client:delete(<<"session">>, State#user.sessionid),
    if 
        State#user.room_pid =/= undefinded ->
            apply(?GAME_ROOM, remove, [State#user.room_pid, State#user.pid])
    end,
    ok.

%%==================================================
%% Internal Functions
%%==================================================

to_json_string(Struct) ->
    iolist_to_binary(encode(Struct)).