-module(ws_user_handler).

-export([init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-include("../include/gs_user.hrl").

-define(GAME_SERVICE, gs_game_service).
-define(GAME_ROOM, gs_game_room).

-import(mochijson2, [encode/1, decode/1]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 300000}}.

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
            Resp = case apply(?GAME_SERVICE, get_room, [Room]) of
                {ok, RoomPid} ->
                    apply(?GAME_ROOM, add, [RoomPid, State#user.pid]),
                    to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(State#user.name)]))}]});
                no_such_room ->
                    case apply(?GAME_SERVICE, new_room, [Room]) of
                        {room_created, RoomPid} ->
                            apply(?GAME_ROOM, add, [RoomPid, State#user.pid]),
                            to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(State#user.name)]))}]});
                        room_already_exists ->
                            to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["The room already exists. Try a different room name,  ", binary_to_list(State#user.name)]))}]})
                    end
            end,
            {reply, {text, <<Resp/binary>>}, State};
        <<"leave_chat_room">> ->
            Room = proplists:get_value(<<"room">>, JsonData),
            Resp = case apply(?GAME_SERVICE, get_room, [Room]) of
                {ok, RoomPid} ->
                    apply(?GAME_ROOM ,remove, [RoomPid, State#user.pid]),
                    to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I think I droped you, ", binary_to_list(State#user.name), " I promise... at some point, I will."]))}]});
                _ ->
                    to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["The room didn't exist, ", binary_to_list(State#user.name), ". Try a different room name."]))}]})
            end,
            {reply, {text, <<Resp/binary>>}, State};
        <<"input">> ->
            Key = proplists:get_value(<<"key">>, JsonData),
            Room = proplists:get_value(<<"room">>, JsonData),
            io:format("Room: ~p~nKey: ~p~n", [Room, Key]),
            GameObjects = to_json_string({struct, [{<<"game_objects">>, {struct, [{<<"root">>, {struct, [{<<"x">>, 0}, {<<"y">>, 0}]} }]} }]}),
            {reply, {text, <<GameObjects/binary>>}, State};
        <<"start_game">> ->
            apply(?GAME_ROOM, start_game, []),
            Resp = to_json_string({struct, [{<<"msg">>, <<"I will start your game once it's implemented...">>}]}),
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
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _ConnState, State) ->
    io:format("~p - Terminated with reason: ~p~n", [self(), Reason]),
    gs_riak_client:delete(<<"session">>, State#user.sessionid),
    ok.

%%==================================================
%% Internal Functions
%%==================================================

to_json_string(Struct) ->
    iolist_to_binary(encode(Struct)).