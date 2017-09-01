-module(gs_chat_service).

-behaviour(gen_server).

%% API
-export([start_link/0,
         % User functions
         new_user/1,
         get_user/1,
         change_user_name/2,
         delete_user/1,
         % Room functions
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

% The globally required details of a room
-record(room, {name, pid}).

% The nameserver's state
-record(state, {users = ets:new(users, [{keypos, #user.name}]),
                rooms = ets:new(rooms, [{keypos, #room.name}])}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Add a new user to the server and give them a name automatically
%%
%% @spec new_user() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
new_user(UserName) ->
    gen_server:call(?SERVER, {new_user, UserName}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a user's pid based on their name
%%
%% @spec get_user(UserName) -> {ok, UPid} | no_such_user
%% @end
%%--------------------------------------------------------------------
get_user(UserName) ->
  gen_server:call(?SERVER, {get_user, UserName}).

%%--------------------------------------------------------------------
%% @doc
%% Change a user's name in the name server
%%
%% @spec change_user_name(UPid, NewName) -> ok | name_taken
%% @end
%%--------------------------------------------------------------------
change_user_name(OldName, NewName) ->
  gen_server:call(?SERVER, {change_user_name, OldName, NewName}).

%%--------------------------------------------------------------------
%% @doc
%% Delete a user by name from the name server
%%
%% @spec delete_user(Name) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
delete_user(Name) ->
  gen_server:call(?SERVER, {delete_user, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Get the Pid of a room based on its name
%%
%% @spec get_room(RoomName) -> {ok, RoomPid} | no_such_room
%% @end
%%--------------------------------------------------------------------
get_room(RoomName) ->
    gen_server:call(?SERVER, {get_room, RoomName}).

%%--------------------------------------------------------------------
%% @doc
%% Add a new room to the name server
%%
%% @spec new_room(RoomName) -> {ok, RoomPid} | no_such_room
%% @end
%%--------------------------------------------------------------------
new_room(RoomName) ->
    gen_server:call(?SERVER, {new_room, RoomName}).

%%--------------------------------------------------------------------
%% @doc
%% Delete a room by name from the name server
%%
%% @spec delete_room(RoomName) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
delete_room(RoomName) ->
  gen_server:call(?SERVER, {delete_room, RoomName}).

%%--------------------------------------------------------------------
%% @doc
%% Get the contents of a room by name
%%
%% @spec get_room_contents(RoomName) -> {ok, Contents, Owner}
%%                                   |  {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_room_contents(RoomName) ->
  gen_server:call(?SERVER, {room_contents, RoomName}).

%%--------------------------------------------------------------------
%% @doc
%% List all rooms in the server with the number of occupants in each
%%
%% @spec list_rooms() -> {ok, RoomList} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
list_rooms() ->
  gen_server:call(?SERVER, list_rooms).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    State = #state{},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({new_user, UserName}, {UPid, _Tag}, State) ->
    User = #user{pid = UPid, name = UserName},
    NewState = add_user(State, User),
    {reply, {ok, UserName}, NewState};

handle_call({get_user, UserName}, _From, State) ->
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
            {ok, RoomPid}  = gs_chat_room:new(RoomId, UPid),
            Room     = #room{name = RoomId, pid = RoomPid},
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
      {Contents, Owner} = gs_caht_room:get_contents(RoomPid),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% User functions
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add a new user to the user name table
%%
%% @spec add_user(State, UserRecord) -> #state{}
%% @end
%%--------------------------------------------------------------------
add_user(State, User) ->
  ets:insert_new(State#state.users, User),
  State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove a user from the user name table
%%
%% @spec remove_user(State, Username) -> #state{}
%% @end
%%--------------------------------------------------------------------
remove_user(State, UserName) ->
  ets:delete(State#state.users, UserName),
  State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Test whether a user name is already recorded in the nameserver
%%
%% @spec user_name_exists(State, UserName) -> boolean()
%% @end
%%--------------------------------------------------------------------
user_name_exists(State, User) ->
  ets:member(State#state.users, User).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the pid of a user based on their name
%%
%% @spec get_user_pid(State, UserName) -> {ok, pid()} | no_such_user
%% @end
%%--------------------------------------------------------------------
get_user_pid(State, UserName) ->
  case ets:lookup(State#state.users, UserName) of
    [User] -> {ok, User#user.pid};
    []     -> no_such_user
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set the name of a user identified by a binary to the given binary
%%
%% @spec set_user_name(State, OldName, NewName) -> {ok, #state{}}
%%                                              |  {error, Reason}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the process identifier for a room based on its name
%%
%% @spec get_room_pid(State, RoomName) -> {ok, pid()} | no_such_room
%% @end
%%--------------------------------------------------------------------
get_room_pid(State, RoomName) ->
  RoomTable = State#state.rooms,
  case ets:lookup(RoomTable, RoomName) of
    [#room{pid=Pid}] ->
        {ok, Pid};
    [] ->
        no_such_room
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a room to the room table if one with the same name does not
%% already exist
%%
%% @spec add_room(State, Room) -> {ok, #state{}} | {duplicate, #state{}}
%% @end
%%--------------------------------------------------------------------
add_room(State, Room) ->
  RoomTable = State#state.rooms,
  case ets:insert_new(RoomTable, Room) of
    true ->
      {ok, State};
    _ ->
      {duplicate, State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes a room from the room name table
%%
%% @spec remove_room(State, RoomName) -> state{}
%% @end
%%--------------------------------------------------------------------
remove_room(State, RoomName) ->
  ets:delete(State#state.users, RoomName),
  State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if a room with the given name already exists, false
%% otherwise
%%
%% @spec room_exists(State, RoomName) -> boolean()
%% @end
%%--------------------------------------------------------------------
room_exists(State, RoomName) ->
  RoomTable = State#state.rooms,
  ets:member(RoomTable, RoomName).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compile a list of room names and number of occupants
%% 
%% @spec compile_room_list(State) ->
%%               [#{roomid => RoomName, count => OccupantCount}]
%% @end
%%--------------------------------------------------------------------
compile_room_list(State) ->
  GetListing = fun (Room, Acc) ->
                   NumOcc = gs_chat_room:count_occupants(Room#room.pid),
                   Summary = #{roomid => Room#room.name, count => NumOcc},
                   [Summary | Acc]
               end,
  ets:foldl(GetListing, [], State#state.rooms).