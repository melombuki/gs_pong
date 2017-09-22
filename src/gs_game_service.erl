-module(gs_game_service).

-behaviour(gen_server).

%% API
-export([start_link/0,
         new_user/1,
         get_user_name/1,
         get_user_pid/1,
         change_user_name/2,
         delete_user/1,
         get_room/1,
         new_room/1,
         delete_room/1,
         get_room_contents/1,
         list_rooms/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/gs_user.hrl").

-define(SERVER, ?MODULE).
-define(GAME_ROOM, gs_game_room).

-record(room, {name, pid}).

-record(state, {users = ets:new(users, [{keypos, #user.name}]),
                rooms = ets:new(rooms, [{keypos, #room.name}])}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_user(UserName) ->
    gen_server:call(?SERVER, {new_user, UserName}).

get_user_name(UserPid) ->
  gen_server:call(?SERVER, {get_user_pid, UserPid}).

get_user_pid(UserName) ->
  gen_server:call(?SERVER, {get_user_name, UserName}).

change_user_name(OldName, NewName) ->
  gen_server:call(?SERVER, {change_user_name, OldName, NewName}).

delete_user(Name) ->
  gen_server:call(?SERVER, {delete_user, Name}).

get_room(RoomName) ->
    gen_server:call(?SERVER, {get_room, RoomName}).

new_room(RoomName) ->
    gen_server:call(?SERVER, {new_room, RoomName}).

delete_room(RoomName) ->
  gen_server:call(?SERVER, {delete_room, RoomName}).

get_room_contents(RoomName) ->
  gen_server:call(?SERVER, {room_contents, RoomName}).

list_rooms() ->
  gen_server:call(?SERVER, list_rooms).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    {ok, #state{}}.

handle_call({new_user, UserName}, {UPid, _Tag}, State) ->
    User = #user{pid = UPid, name = UserName},
    NewState = add_user(State, User),
    {reply, {ok, UserName}, NewState};

handle_call({get_user_name, UserPid}, _From, State) ->
  Reply = get_user_name(State, UserPid),
  {reply, Reply, State};

handle_call({get_user_pid, UserName}, _From, State) ->
  Reply = get_user_pid(State, UserName),
  {reply, Reply, State};

handle_call({change_user_name, OldName, NewName}, _From, State) ->
  case user_name_exists(State, NewName) of
    true ->
      {reply, name_taken, State};
    _    ->
      NextState = set_user_name(State, OldName, NewName),
      {reply, ok, NextState}
  end;

handle_call({delete_user, UserName}, _From, State) ->
  case user_name_exists(State, UserName) of
    true ->
      NewState = remove_user(State, UserName),
      {reply, ok, NewState};
    _    ->
      {reply, {error, no_such_user}, State}
  end;

handle_call({get_room, RoomName}, _From, State) ->
    Reply = get_room_pid(State, RoomName),
    {reply, Reply, State};

handle_call({new_room, RoomId}, {UPid, _Tag}, State) ->
    {Reply, State1} = case room_exists(State, RoomId) of
        true -> 
            {room_already_exists, State};
        _    ->
            {ok, RoomPid}  = apply(?GAME_ROOM, new, [RoomId, UPid]),
            Room = #room{name = RoomId, pid = RoomPid},
            {ok, NewState} = add_room(State, Room),
            {{room_created, RoomPid}, NewState}
  end,
  {reply, Reply, State1};

handle_call({delete_room, RoomName}, _From, State) ->
  case room_exists(State, RoomName) of
    true ->
      NewState = remove_room(State, RoomName),
      {reply, ok, NewState};
    _ ->
      {reply, {error, no_such_room}, State}
  end;

handle_call({room_contents, RoomName}, _From, State) ->
  case room_exists(State, RoomName) of
    true ->
      RoomPid = get_room_pid(State, RoomName),
      {Contents, Owner} = apply(?GAME_ROOM, get_contents, [RoomPid]),
      {reply, {ok, Contents, Owner}, State};
    _ ->
      {reply, {error, no_such_room}, State}
  end;

handle_call(list_rooms, _From, State) ->
  RoomList = compile_room_list(State),
  {reply, {ok, RoomList}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% User functions
%%%-------------------------------------------------------------------

add_user(State, User) ->
  ets:insert_new(State#state.users, User),
  State.

remove_user(State, UserName) ->
  ets:delete(State#state.users, UserName),
  State.

user_name_exists(State, User) ->
  ets:member(State#state.users, User).

get_user_pid(State, UserName) ->
  case ets:lookup(State#state.users, UserName) of
    [User] -> {ok, User#user.pid};
    []     -> no_such_user
  end.

% TODO - get the user name
get_user_name(_State, _UserPid) ->
%   case ets:lookup(State#state.users, UserPid) of
%     [User] -> {ok, User#user.pid};
%     []     -> no_such_user
%   end.
    {ok, <<"Ah ah ah, not implemented">>}.

set_user_name(State, OldName, NewName) ->
  UserTable = State#state.users,
  case ets:take(UserTable, OldName) of
    [User] ->
      NewUser = User#user{name = NewName},
      case ets:insert_new(UserTable, NewUser) of
        true ->
          {ok, State};
        _    ->
          {error, username_taken}
      end;
    []     ->
      {error, no_such_user};
    _      ->
      {error, multiple_users_with_same_name}
  end.

%%%-------------------------------------------------------------------
%%% Room functions
%%%-------------------------------------------------------------------

get_room_pid(State, RoomName) ->
  RoomTable = State#state.rooms,
  case ets:lookup(RoomTable, RoomName) of
    [#room{pid=Pid}] ->
        {ok, Pid};
    [] ->
        no_such_room
  end.

add_room(State, Room) ->
  RoomTable = State#state.rooms,
  case ets:insert_new(RoomTable, Room) of
    true ->
      {ok, State};
    _ ->
      {duplicate, State}
  end.

remove_room(State, RoomName) ->
  ets:delete(State#state.users, RoomName),
  State.

room_exists(State, RoomName) ->
  RoomTable = State#state.rooms,
  ets:member(RoomTable, RoomName).

compile_room_list(State) ->
  GetListing = fun (Room, Acc) ->
                   NumOcc = apply(?GAME_ROOM, count_occupants, [Room#room.pid]),
                   Summary = #{roomid => Room#room.name, count => NumOcc},
                   [Summary | Acc]
               end,
  ets:foldl(GetListing, [], State#state.rooms).
  