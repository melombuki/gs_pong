-module(ws_user_handler).

-export([init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-include("../include/gs_user.hrl").

-import(mochijson2, [encode/1, decode/1]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 300000}}.

websocket_init(State) ->
    State1 = State#user{pid = self()},
    {ok, State1}.

websocket_handle({text, Msg}, State) ->
    {struct, JsonData} = decode(Msg),
    case proplists:get_value(<<"type">>, JsonData) of
        <<"select_username">> ->
            Nickname = proplists:get_value(<<"nick">>, JsonData),
            {ok, Name} = gs_chat_service:new_user(Nickname),
            Resp = to_json_string({struct,[{<<"msg">>, Name}]}),
            {reply, {text, <<Resp/binary>>}, State};
        <<"chat_msg">> ->
            UserMsg = binary_to_list(proplists:get_value(<<"msg">>, JsonData)),
            % Nickname = proplists:get_value(<<"nick">>, JsonData),
            Room = proplists:get_value(<<"room">>, JsonData),
            case gs_chat_service:get_room(Room) of
                {ok, RoomPid} ->
                    % TODO add the sending users name to the broadcast
                    gs_chat_room:broadcast(RoomPid, UserMsg),
                    {ok, State};
                no_such_room ->
                    Resp = to_json_string({struct,[{<<"msg">>, list_to_binary("Failed to send your message.")}]}),
                    {reply, {text, <<Resp/binary>>}, State}
            end;
        <<"join_chat_room">> ->
            Room = proplists:get_value(<<"room">>, JsonData),
            Nickname = proplists:get_value(<<"nick">>, JsonData),
            Resp = case gs_chat_service:get_room(Room) of
                {ok, RoomPid} ->
                    gs_chat_room:add(RoomPid, State#user.pid),
                    to_json_string({struct,[{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(Nickname)]))}]});
                no_such_room ->
                    case gs_chat_service:new_room(Room) of
                        {room_created, RoomPid} ->
                            gs_chat_room:add(RoomPid, State#user.pid),
                            to_json_string({struct,[{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(Nickname)]))}]});
                        room_already_exists ->
                            to_json_string({struct,[{<<"msg">>, list_to_binary(lists:concat(["The room already exists. Try a different room name,  ", binary_to_list(Nickname)]))}]})
                    end
            end,
            {reply, {text, <<Resp/binary>>}, State};
        <<"leave_chat_room">> ->
            Room = proplists:get_value(<<"room">>, JsonData),
            Nickname = proplists:get_value(<<"nick">>, JsonData),
            Resp = case gs_chat_service:get_room(Room) of
                {ok, RoomPid} ->
                    gs_chat_room:remove(RoomPid, State#user.pid),
                    to_json_string({struct,[{<<"msg">>, list_to_binary(lists:concat(["I think I droped you, ", binary_to_list(Nickname), " I promise... at some point, I will."]))}]});
                _ ->
                    to_json_string({struct,[{<<"msg">>, list_to_binary(lists:concat(["The room didn't exist, ", binary_to_list(Nickname), ". Try a different room name."]))}]})
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
websocket_info({broadcast, Msg}, State) ->
    io:format("websocket_info ~p~n", [Msg]),
    {reply, {text, to_json_string({struct, [{<<"msg">>, list_to_binary(Msg)}]})}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _ConnState, _State) -> 
    io:format("~p - Terminated with reason: ~p~n", [self(), Reason]),
    ok.

%%==================================================
%% Internal Functions
%%==================================================

to_json_string(Struct) ->
    iolist_to_binary(encode(Struct)).
