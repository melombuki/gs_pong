-module(ws_user_handler).

-export([init/2, 
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-include("../include/gs_user.hrl").

-import(mochijson2, [encode/1, decode/1]).

init(Req, State) ->
    io:format("~p~n", [Req]),
    {cowboy_websocket, Req, State, #{idle_timeout => 300000}}.

websocket_init(State) ->
    State1 = State#user{pid = self()},
    {ok, State1}.

websocket_handle({text, Msg}, State) ->
    {struct, JsonData} = decode(Msg),
    case proplists:get_value(<<"type">>, JsonData) of
        <<"chat_msg">> ->
            UserMsg = binary_to_list(proplists:get_value(<<"msg">>, JsonData)),
            Nickname = proplists:get_value(<<"nick">>, JsonData),
            Resp = to_json_string({struct,[{<<"msg">>, list_to_binary(UserMsg ++ " , right back at ya.")}]}),
            {reply, {text, <<Resp/binary>>}, State};
        <<"join_chat_group">> ->
            Group = proplists:get_value(<<"nick">>, JsonData),
            Nickname = proplists:get_value(<<"nick">>, JsonData),
            Resp = to_json_string({struct,[{<<"msg">>, list_to_binary(lists:concat(["I will add you, ", binary_to_list(Nickname), " I promise... at some point, I will."]))}]}),
            {reply, {text, <<Resp/binary>>}, State};
        <<"leave_chat_group">> ->
            Group = proplists:get_value(<<"nick">>, JsonData),
            Nickname = proplists:get_value(<<"nick">>, JsonData),
            Resp = to_json_string({struct,[{<<"msg">>, list_to_binary(lists:concat(["I will drop you, ", binary_to_list(Nickname), " I promise... at some point, I will."]))}]}),
            {reply, {text, <<Resp/binary>>}, State};
        _ ->
            Resp = to_json_string({struct, [{<<"msg">>, <<"I didn't quite get that.">>}]}),
            {reply, {text, << Resp/binary >>}, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
    {ok, State, hibernate};
websocket_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
    {ok, State}.

terminate(Reason, _ConnState, _State) -> 
    io:format("self() = ~p. Terminated with reason: ~p~n", [self(), Reason]),
    ok.

%%==================================================
%% Internal Functions
%%==================================================

to_json_string(Struct) ->
    iolist_to_binary(encode(Struct)).
