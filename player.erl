-module(player).
-behaviour(gen_fsm).

-define(PLAYER, player).
-define(LOG, loggen).
-define(LOG_MODULE, log).

%%Public API
-export([new/1, close/0, enter_room/0, leave_room/0, move/1, state/0]).
%%FSM API
-export([init/1, handle_event/3, handle_info/3, handle_sync_event/4, code_change/4, terminate/3]).
%%States
-export([alive/2, limbo/2]). %dead/2,

%%--------------------------------------------------------------------
%% @doc Create a player and sends him to the limbo.
%% @end
%% -------------------------------------------------------------------
-spec new(Name :: atom()) -> ok.
new(Name) ->
  gen_fsm:start_link({local, ?PLAYER}, ?MODULE, Name, []).

%%--------------------------------------------------------------------
%% @doc Asks for enter a room to start playing
%% @end
%% -------------------------------------------------------------------
-spec enter_room() -> ok.
enter_room()->
    gen_fsm:send_event(?PLAYER,{enter}).

%%--------------------------------------------------------------------
%% @doc Leave the room and go to the lobby
%% @end
%% -------------------------------------------------------------------
-spec leave_room() -> ok.
leave_room()->
    gen_fsm:send_event(?PLAYER,{leave}).

%%--------------------------------------------------------------------
%% @doc Print the state and the score of the player
%% @end
%% -------------------------------------------------------------------
-spec state() -> ok.
state() ->
  gen_fsm:send_all_state_event(?PLAYER,state).

%%--------------------------------------------------------------------
%% @doc Exits the game
%% @end
%% -------------------------------------------------------------------
-spec close() -> ok.
close() ->
  gen_fsm:send_all_state_event(?PLAYER, stop).

%%--------------------------------------------------------------------
%% @doc Ask the room for moving the player
%% @end
%% -------------------------------------------------------------------
-spec move(M :: atom()) -> ok.
move(M)->
  case lists:member(M, [u,d,r,l]) of
    true ->
      gen_fsm:send_event(?PLAYER,{move, M});
    false ->
      io:format("Not moving :(")
  end.

%%====================================================================
init(Name) ->
  io:format("Welcome, ~p ~n", [Name]),
  {ok, _Pid} = gen_event:start_link({local, ?LOG}),
  gen_event:add_handler(?LOG, ?LOG_MODULE, atom_to_list(Name) ++ "_log.txt"),
	gen_event:notify(?LOG, {save_log, {"Nuevo Jugador"}}),
  {ok, limbo, {Name, 0, -1}}.

limbo({enter},{Name, 0, -1})->
  io:format("I'm ~p and I want to enter in a room!! ~n", [Name]),
  %% Stuff to connect to the server to ask for a room
  {server, server@localhost} ! {enter_room, node(), Name},
  gen_event:notify(?LOG, {save_log, {"Buscando Sala..."}}),
  {next_state,limbo,{Name, 0, -1}}.

alive({leave}, {Name, Score, RoomPid})->
  io:format("I leave the room I was in ~n"),
  gen_event:notify(?LOG, {save_log, {"El jugador se va de la sala."}}),
  {server, server@localhost} ! {exit_room, RoomPid},
  {next_state,limbo,{Name, Score, -1}};

alive({move, Move}, {Name, Score, RoomPid}) ->
  RoomPid ! {move, node(), Move},
  gen_event:notify(?LOG, {save_log, {"Movimiento hacia "++atom_to_list(Move)}}),
  {next_state, alive, {Name, Score, RoomPid}}.

handle_event(state, State, {Name, Score, RoomPid}) ->
  io:format("~p: ~p, puntuación ~p en la sala ~p", [Name, State,Score, RoomPid]),
  {next_state, State, {Name, Score, RoomPid}};

handle_event(stop, _State, _StateEvent) ->
  {stop, normal, []}.

handle_info({room_aceptance, RoomPid}, _State, {Name, Score, _OldRoomPid}) ->
  io:format("Aceptado en la sala ~p~n", [RoomPid]),
  gen_event:notify(?LOG, {save_log, {"Aceptado en la sala "++pid_to_list(RoomPid)}}),
  {next_state, alive, {Name, Score, RoomPid}};

handle_info({results, Move, Near_Enemies, {Battle, Enemy}}, _State, {Name, Score, RoomPid}) ->
  gen_event:notify(?LOG, {save_log, {"Movimiento "++atom_to_list(Move)++", "++lists:flatten(io_lib:format("~p", [Near_Enemies]))
    ++" blind_warrior cerca, "++atom_to_list(Battle)++" en batalla."}}),
  case Move of
    ilegal -> io:format("Has chocado contra una pared.~n");
    _ -> io:format("Has conseguido moverte.~n")
  end,
  io:format("Sientes que hay ~p blind warriors cerca.~n", [Near_Enemies]),

  case Battle of
    win ->
      io:format("Has matado al blind Warrior ~p~n", [Enemy]),
      {next_state, alive, {Name, Score+10, RoomPid}};
    dead ->
      io:format("El Blind Warrior ~p te ha dado caza. ~nHas muerto con ~p puntos.~n",[Enemy, Score]),
      {server, server@localhost} ! {exit_room, RoomPid},
      {next_state, limbo, {Name, Score, -1}};
    peace ->
      {next_state, alive, {Name, Score, RoomPid}}
  end;

handle_info({dead_in_battle,NodeEnemy}, _State, {Name, Score, RoomPid}) ->
  io:format("El Blind Warrior ~p te ha atacado por la espalda. Mueres con ~p puntos", [NodeEnemy,Score]),
  gen_event:notify(?LOG, {save_log, {"Muere en batalla."}}),
  {server, server@localhost} ! {exit_room, RoomPid},
  {next_state, limbo, {Name, Score, -1}};

handle_info(_Info, StateName,StateData ) ->
  {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
  {next_state, StateName, StateData}.

code_change(_Old, StateName, StateData, _) ->
  {ok, StateName, StateData}.

terminate(normal, _State, _StateData) ->
  ok.
