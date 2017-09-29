-module(ws_user_handler).

-export([init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-include("../include/gs_user.hrl").

-import(mochijson2, [encode/1, decode/1]).

init(Req, State) ->
    % {cowboy_websocket, Req, State, #{idle_timeout => 600000}}.
    {cowboy_websocket, Req, State, #{idle_timeout => 60000000}}. % For testing only, DELETE ME!!

websocket_init(State) ->
    {ok, State#user{pid = self()}, hibernate}.

websocket_handle({text, Msg}, State) ->
    {struct, JsonData} = decode(Msg),
    case proplists:get_value(<<"type">>, JsonData) of
        <<"select_username">> ->
            {ok, Name} = gs_game_service:new_user(State#user.name),
            Resp = to_json_string({struct, [{<<"msg">>, Name}]}),
            {reply, {text, <<Resp/binary>>}, State};
        <<"chat_msg">> ->
            UserMsg = binary_to_list(proplists:get_value(<<"msg">>, JsonData)),
            Room = proplists:get_value(<<"room">>, JsonData),
            % TODO - get this from State and check for undefined... not user input 
            case gs_game_service:get_room(Room) of
                {ok, RoomPid} ->
                    % TODO add the sending users name to the broadcast
                    gs_game_room:chat_broadcast(RoomPid, State#user.name, UserMsg),
                    {ok, State};
                no_such_room ->
                    Resp = to_json_string({struct, [{<<"msg">>, list_to_binary("Failed to send your message.")}]}),
                    {reply, {text, <<Resp/binary>>}, State}
            end;
        <<"join_game_room">> ->
            Room = proplists:get_value(<<"room">>, JsonData),
            case gs_game_service:get_room(Room) of
                {ok, RoomPid} ->
                    ok = gs_game_room:add(RoomPid, State#user.pid),
                    State1 = State#user{room_name = Room, room_pid = RoomPid},
                    Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(State1#user.name)]))}]}),
                    {reply, {text, <<Resp/binary>>}, State1};
                no_such_room ->
                    case gs_game_service:new_room(Room) of
                        {room_created, RoomPid} ->
                            State1 = State#user{room_name = Room, room_pid = RoomPid},
                            gs_game_room:add(RoomPid, State#user.pid),
                            Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually added you, ", binary_to_list(State1#user.name)]))}]}),
                            {reply, {text, <<Resp/binary>>}, State1};
                        room_already_exists ->
                            Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["The room already exists. Try a different room name,  ", binary_to_list(State#user.name)]))}]}),
                            {reply, {text, <<Resp/binary>>}, State}
                    end
            end;
        <<"leave_game_room">> ->
            leave_game_room(State#user.room_name, State#user.room_pid, State#user.pid),
            Resp = to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["I actually dropped you, ", binary_to_list(State#user.name)]))}]}),
            {reply, {text, <<Resp/binary>>}, State};
        <<"input">> ->
            Key = proplists:get_value(<<"key">>, JsonData),
            gs_game_room:handle_input(State#user.room_pid, State#user.pid, Key),
            {ok, State};
        <<"start_game">> ->
            Resp = case State#user.room_pid of
                RoomPid when State#user.room_pid =/= undefined ->
                    % TODO add the sending users name to the broadcast
                    gs_game_room:start_game(RoomPid),
                    to_json_string({struct, [{<<"msg">>, <<"Started your game">>}]});
                _ ->
                    to_json_string({struct, [{<<"msg">>, list_to_binary(lists:concat(["You are not in a game room, ", binary_to_list(State#user.name), ". Try a different room name."]))}]})
            end,
            {reply, {text, <<Resp/binary>>}, State};
        <<"stop_game">> ->
            Resp = case State#user.room_pid of
                RoomPid when State#user.room_pid =/= undefined ->
                    % TODO add the sending users name to the broadcast
                    gs_game_room:stop_game(RoomPid),
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
websocket_info({chat_broadcast, UserName, Msg}, State) ->
    Style = case State#user.name of
        UserName ->
            <<"me">>;
        _ ->
            <<"them">>
    end,
    {reply, {text, to_json_string({struct, [{<<"from">>, UserName}, {<<"style">>, Style}, {<<"msg">>, list_to_binary(Msg)}]})}, State};
websocket_info({game_objects, Root}, State) ->
    GameObjects = gs_game_object:to_proplist(Root),
    Resp = to_json_string({struct, [{<<"game_objects">>, GameObjects}]}),
    {reply, {text, <<Resp/binary>>}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _ConnState, State) ->
    io:format("~p - Terminated with reason: ~p~n", [self(), Reason]),
    gs_riak_client:delete(<<"session">>, State#user.sessionid),
    leave_game_room(State#user.room_name, State#user.room_pid, State#user.pid),
    ok.

%%==================================================
%% Internal Functions
%%==================================================

to_json_string(Struct) ->
    iolist_to_binary(encode(Struct)).

leave_game_room(RoomName, RoomPid, UserPid) ->
    case gs_game_service:get_room(RoomName) of
        {ok, _} ->
            gs_game_room:remove(RoomPid, UserPid);
        _ ->
            ok
    end.