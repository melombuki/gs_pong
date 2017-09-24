-module(gs_game_room).

-behaviour(gen_server).

% API
-export([new/2,
         chat_broadcast/2,
         start_game/1,
         stop_game/1,
         add/2,
         remove/2,
         delete/2,
         handle_input/3]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(mochijson2, [encode/1]).

-define(SERVER, ?MODULE).
-define(GAME_OBJECT, gs_game_object).
-define(PADDLE_MIN_Y, 0).
-define(PADDLE_MAX_Y, 200).
-define(PADDLE_STEP_Y, 4).
-define(CANVAS_WIDTH, 796).
-define(CANVAS_HEIGHT, 296).

-include("../include/gs_game_object.hrl").

-record(state, {roomid, users = #{}, owner = <<>>, game_objects = ets:new(game_objects, [{keypos, #game_object.name}]), tref, ball_speed_x = 4, ball_speed_y = 4}).
-record(player, {player_number}).

%%%===================================================================
%%% TODOs
%%%===================================================================
%   - Ball movement
%   - Collision test for player walls, paddles etc.
%   - Game over handling

%%%===================================================================
%%% API
%%%===================================================================

new(RoomId, Owner) ->
    gen_server:start_link(?MODULE, [RoomId, Owner], []).

chat_broadcast(RoomId, Msg) ->
    gen_server:cast(RoomId, {chat_broadcast, Msg}).

get_game_objects(RoomId) ->
    gen_server:call(RoomId, get_game_objects).

handle_input(RoomId, UserPid, Key) ->
    gen_server:cast(RoomId, {handle_input, UserPid, Key}).

start_game(RoomId) ->
    gen_server:cast(RoomId, start_game).

stop_game(RoomId) ->
    gen_server:cast(RoomId, stop_game).

add(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {add, UserPid}).

remove(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove, UserPid}).

delete(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {delete, UserPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([RoomId, Owner]) ->
    {ok, #state{roomid = RoomId, owner = Owner}}.

handle_call({add, UPid}, _From, State) ->
    NewState = add_user(State, UPid),
    {reply, ok, NewState};

handle_call({remove, UPid}, _From, State) ->
    NewState = remove_user(State, UPid),
    case maps:size(NewState#state.users) of
        0 ->
            {stop, shutdown, ok, State};
        _ ->
            {reply, ok, NewState}
    end;

handle_call({delete, _SenderName}, _From, State) ->
    Msg = #{type => leave_room},
    send_all_msg(State#state.users, Msg),
    {stop, shutdown, ok, State};

handle_call(get_game_objects, _From, State) ->
    {reply, State#state.game_objects, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({chat_broadcast, Msg}, State) ->
    send_all_msg(State#state.users, Msg),
    {noreply, State};

handle_cast({handle_input, UserPid, Key}, State) ->
    Player = maps:get(UserPid, State#state.users),
    Paddle = case Player#player.player_number of
        0 -> apply(?GAME_OBJECT, get_game_object, [State#state.game_objects, ?PADDLE1]);
        1 -> apply(?GAME_OBJECT, get_game_object, [State#state.game_objects, ?PADDLE2]);
        _ -> apply(?GAME_OBJECT, get_game_object, [State#state.game_objects, ?PADDLE1])
    end,
    UpdatedY = case Key of
        <<"ArrowUp">> ->
            max(?PADDLE_MIN_Y, Paddle#game_object.y - ?PADDLE_STEP_Y);
        <<"ArrowDown">> ->
            min(?PADDLE_MAX_Y, Paddle#game_object.y + ?PADDLE_STEP_Y);
        _ ->
            0
    end,
    apply(?GAME_OBJECT, position, [State#state.game_objects, Paddle, Paddle#game_object.x, UpdatedY]),
    {noreply, State};

handle_cast(start_game, State) ->
    ok = init_game_objects(State#state.game_objects),
    io:format("~p~n", ["Starting the game..."]),
    % Approx 15 messages per second...
    {ok, TRef} = timer:send_interval(64, self(), tick_game_state),
    {noreply, State#state{tref = TRef}};

handle_cast(stop_game, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    io:format("~p~n", ["Stoping the game..."]),
    {noreply, State#state{tref = undefined}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick_game_state, State) ->
    State1 = tick_game_state(State#state.users, State),
    {noreply, State1};

handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% internal functions
%%%===================================================================

add_user(State, UserPid) ->
    PlayerState = #player{player_number = maps:size(State#state.users)},
    UpdatedUsers = maps:put(UserPid, PlayerState, State#state.users),
    State#state{users = UpdatedUsers}.

remove_user(State, UserPid) ->
    io:format("User ~p left the game~n", [UserPid]),
    UpdatedUsers = maps:remove(UserPid, State#state.users),
    State#state{users = UpdatedUsers}.

send_all_msg(UserMap, Message) ->
    SendOne = fun (UPid, _) ->
        UPid ! {chat_broadcast, Message}
    end,
    maps:map(SendOne, UserMap),
    ok.

send_all_game_objects(UserMap, GameObjects) ->
    SendOne = fun (UPid, _) ->
        UPid ! {game_objects, GameObjects}
    end,
    maps:map(SendOne, UserMap),
    ok.

tick_game_state(UserMap, State) ->
    State1 = tick_ball(State),
    send_all_game_objects(UserMap, apply(?GAME_OBJECT, get_game_tree, [State#state.game_objects])),
    State1.

init_game_objects(Table) ->
    {ok, Paddle1} = apply(?GAME_OBJECT, new, [{?PADDLE1, 10, 0, 20, 100, "white", true, []}]),
    {ok, Paddle2} = apply(?GAME_OBJECT, new, [{?PADDLE2, 770, 0, 20, 100, "white", true, []}]),
    {ok, Ball}    = apply(?GAME_OBJECT, new, [{?BALL, 400, 150, 10, 10, "blue", true, []}]),
    {ok, Root}    = apply(?GAME_OBJECT, new, [{?ROOT, 0, 0, 0, 0, "black", false, []}]),
    ets:insert(Table, Paddle1),
    ets:insert(Table, Paddle2),
    ets:insert(Table, Ball),
    ets:insert(Table, Root),
    ok.

tick_ball(State) ->
    Ball = apply(?GAME_OBJECT, get_game_object, [State#state.game_objects, ?BALL]),
    State1 = if
        (Ball#game_object.y + State#state.ball_speed_y > ?CANVAS_HEIGHT) or (Ball#game_object.y + State#state.ball_speed_y < 0) -> 
            State#state{ball_speed_y = -State#state.ball_speed_y};
        true ->
            State
    end,
    State2 = if 
        (Ball#game_object.x + State#state.ball_speed_x > ?CANVAS_WIDTH) or (Ball#game_object.x + State#state.ball_speed_x < 0) -> 
            State1#state{ball_speed_x = -State1#state.ball_speed_x};
        true ->
            State1
    end,
    UpdatedX = Ball#game_object.x + State2#state.ball_speed_x,
    UpdatedY = Ball#game_object.y + State2#state.ball_speed_y,
    State3 = case test_ball_collisions(State2#state.game_objects, Ball, UpdatedX, UpdatedY) of
        none ->
            State2;
        {paddle, Collision} ->
            State1#state{ball_speed_x = -State1#state.ball_speed_x};
        {goal, Collision} ->
            State2
    end,
    apply(?GAME_OBJECT, position, [State3#state.game_objects, Ball, UpdatedX, UpdatedY]),
    State3.

test_ball_collisions(Table, Ball, X, Y) ->
    ObjectsToTest = lists:map(fun (X) -> apply(?GAME_OBJECT, get_game_object, [Table, X]) end, [?PADDLE1, ?PADDLE2]),
    Pred = fun(Other) -> test_ball_collision(Ball, X, Y, Other) end,
    MaybeMatch = firstmatch(ObjectsToTest, Pred),
    case MaybeMatch of
        none ->
            none;
        _ ->
            {paddle, MaybeMatch}
    end.

test_ball_collision(Ball, X, Y, Other) ->
    Result = not ((X < Other#game_object.x + Other#game_object.width)
                    and (X + Ball#game_object.width > Other#game_object.x)
                    and (Y < Other#game_object.y + Other#game_object.height)
                    and (Y + Ball#game_object.height > Other#game_object.y)),
    Result.

firstmatch(List, Pred) -> 
   case lists:dropwhile(Pred, List) of
     [] -> none;
     [X | _] -> X
   end.