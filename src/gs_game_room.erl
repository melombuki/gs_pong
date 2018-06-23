-module(gs_game_room).

-behaviour(gen_server).

% API
-export([new/2,
         chat_broadcast/3,
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

-define(PADDLE_MIN_Y, 0).
-define(PADDLE_MAX_Y, 200).
-define(PADDLE_STEP_Y, 4).
-define(CANVAS_WIDTH, 800).
-define(CANVAS_HEIGHT, 300).

-include("../include/gs_game_object.hrl").

-record(state, {roomid,
                users = #{}, 
                owner = <<>>,
                game_objects = ets:new(game_objects, [{keypos, #game_object.name}]),
                tref, 
                ball_speed_x = 4, 
                ball_speed_y = 4}).

-record(player, {player_number,
                 points = 0}).

%%%===================================================================
%%% TODOs
%%%===================================================================
%   - Points handling
%   - Game over handling

%%%===================================================================
%%% API
%%%===================================================================

new(RoomId, Owner) ->
    gen_server:start_link(?MODULE, [RoomId, Owner], []).

chat_broadcast(RoomId, UserName, Msg) ->
    gen_server:cast(RoomId, {chat_broadcast, UserName, Msg}).

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

handle_call({delete, SenderName}, _From, State) ->
    Msg = #{type => leave_room},
    send_all_msg(State#state.users, SenderName, Msg),
    {stop, shutdown, ok, State};

handle_call(get_game_objects, _From, State) ->
    {reply, State#state.game_objects, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({chat_broadcast, UserName, Msg}, State) ->
    send_all_msg(State#state.users, UserName, Msg),
    {noreply, State};

handle_cast({handle_input, UserPid, Key}, State) ->
    Player = maps:get(UserPid, State#state.users),
    Paddle = case Player#player.player_number of
        0 -> gs_game_object:get_game_object(State#state.game_objects, ?PADDLE1);
        1 -> gs_game_object:get_game_object(State#state.game_objects, ?PADDLE2);
        _ -> gs_game_object:get_game_object(State#state.game_objects, ?PADDLE1)
    end,
    UpdatedY = case Key of
        <<"ArrowUp">> ->
            max(?PADDLE_MIN_Y, Paddle#game_object.y - ?PADDLE_STEP_Y);
        <<"ArrowDown">> ->
            min(?PADDLE_MAX_Y, Paddle#game_object.y + ?PADDLE_STEP_Y);
        _ ->
            0
    end,
    gs_game_object:position(State#state.game_objects, Paddle, Paddle#game_object.x, UpdatedY),
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

send_all_msg(UserMap, UserName, Message) ->
    SendOne = fun (UPid, _) ->
        UPid ! {chat_broadcast, UserName, Message}
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
    send_all_game_objects(UserMap, gs_game_object:get_game_tree(State#state.game_objects)),
    State1.

init_game_objects(Table) ->
    {ok, Paddle1} = gs_game_object:new({?PADDLE1,  10, 100, 20, 100, "white", true, []}),
    {ok, Paddle2} = gs_game_object:new({?PADDLE2, 770, 100, 20, 100, "white", true, []}),
    {ok, Ball}    = gs_game_object:new({?BALL, 400, 150, 10, 10, "blue", true, []}),
    {ok, Root}    = gs_game_object:new({?ROOT, 0, 0, 0, 0, "black", false, []}),
    ets:insert(Table, Paddle1),
    ets:insert(Table, Paddle2),
    ets:insert(Table, Ball),
    ets:insert(Table, Root),
    ok.

tick_ball(State) ->
    Ball = gs_game_object:get_game_object(State#state.game_objects, ?BALL),
    UpdatedX = Ball#game_object.x + State#state.ball_speed_x,
    UpdatedY = Ball#game_object.y + State#state.ball_speed_y,
    State1 = case test_ball_collisions(State#state.game_objects, Ball, UpdatedX, UpdatedY, State#state.ball_speed_x, State#state.ball_speed_y) of
        top_bottom ->
            gs_game_object:position(State#state.game_objects, Ball, UpdatedX, UpdatedY),
            State#state{ball_speed_y = -State#state.ball_speed_y};
        left ->
            ok = init_game_objects(State#state.game_objects),
            reset_ball_position(State#state.game_objects, Ball),
            State#state{ball_speed_x = -State#state.ball_speed_x};
        right ->
            ok = init_game_objects(State#state.game_objects),
            reset_ball_position(State#state.game_objects, Ball),
            State#state{ball_speed_x = -State#state.ball_speed_x};
        paddle ->
            gs_game_object:position(State#state.game_objects, Ball, UpdatedX, UpdatedY),
            State#state{ball_speed_x = -State#state.ball_speed_x};
        none ->
            gs_game_object:position(State#state.game_objects, Ball, UpdatedX, UpdatedY),
            State
    end,
    State1.

test_ball_collisions(Table, Ball, X, Y, BallSpeedX, BallSpeedY) ->
    case test_top_bottom_collision(Ball, Y, BallSpeedY) of
        top_bottom -> top_bottom;
        none ->
            case test_goal_collision(Ball, X, BallSpeedX) of
                left  -> left;
                right -> right;
                none  -> test_object_collisions(Table, Ball, X, Y)
            end
    end.

test_top_bottom_collision(Ball, Y, BallSpeedY) ->
    if 
        (Y + BallSpeedY < 0) 
        or (Y + Ball#game_object.width + BallSpeedY > ?CANVAS_HEIGHT) ->
            top_bottom;
        true -> 
            none
    end.

test_goal_collision(Ball, X, BallSpeedX) ->
    if
        (X + Ball#game_object.width + BallSpeedX > ?CANVAS_WIDTH) ->
            right;
        (X + BallSpeedX < 0) ->
            left;
        true -> none
    end.
        
test_object_collisions(Table, Ball, X, Y) ->
    ObjectsToTest = lists:map(fun (Object) -> gs_game_object:get_game_object(Table, Object) end, [?PADDLE1, ?PADDLE2]),
    Pred = fun(Other) -> test_object_collision(Ball, X, Y, Other) end,
    MaybeMatch = firstmatch(ObjectsToTest, Pred),
    case MaybeMatch of
        none ->
            none;
        _ ->
            paddle
    end.

test_object_collision(Ball, X, Y, Other) ->
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

reset_ball_position(Table, Ball) ->
    R1 = rand:uniform(500) + 100,
    R2 = rand:uniform(200) + 50,
    gs_game_object:position(Table, Ball, R1, R2),
    ok.
