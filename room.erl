-module(room).
%-behaviour(gen_fsm).
-behaviour(gen_server).

-define(ROOM, room).
-define(BOARDSIZE, 5).
-define(VISION, 2).

-define(LOG, loggen).
-define(LOG_MODULE, log).

-export([start_room/1, new_position/0, search_enemies/2, get_player/2]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

%%--------------------------------------------------------------------
%% @doc Start an empty room.
%% @end
%% -------------------------------------------------------------------
-spec start_room(Players::list) -> ok.
start_room(Name) ->
  {ok, Pid} = gen_server:start_link({local, Name}, ?MODULE, [], []),
  io:format("Se crea la sala ~p ~p~n", [Name, Pid]),
  Pid.

%%====================================================================
%%=======================Aux FUNCTIONS================================
%%====================================================================
list_players([]) ->
  ok;
list_players([H|T]) ->
  io:format("~p~n",[H]),
  list_players(T).

new_position() ->
  random:uniform(?BOARDSIZE) -1.

get_player(_Player, []) ->
  error;
get_player(Player, [{P,X,Y,N}|T]) ->
  if
    Player == N -> {P,X,Y,N};
    true -> get_player(Player, T)
  end.

move(X, Y, u) ->
  if
    X > 0 -> {X - 1, Y};
    true -> ilegal
  end;

move(X, Y, d) ->
  if
    X < ?BOARDSIZE -1 -> {X + 1, Y};
    true -> ilegal
  end;

move(X, Y, l) ->
  if
    Y > 0 -> {X, Y-1};
    true -> ilegal
  end;

move(X, Y, r) ->
  if
    Y < ?BOARDSIZE - 1 -> {X, Y+1};
    true -> ilegal
  end;

% Player es el node
move(Player, Move, [{Name, PosX, PosY, Node}|T]) ->
  if
    Player == Node ->
      % Intentamos el movimiento, si es ilegal, devolvemos ilegal, si no players actualizado.
      NM = move(PosX, PosY, Move),
      case NM of
        ilegal -> io:format("Movimiento ilegal~n"),[ilegal];
        {NewPosX, NewPosY} -> io:format("Nos movemos~n"), [{Name, NewPosX, NewPosY, Node}|T]
      end;
    true ->
      [{Name, PosX, PosY, Node}|move(Player, Move, T)]
  end.

search_enemies({_P1, _X1, _Y1, _Node}, []) ->
  0;
search_enemies({P1, X1, Y1, N1}, [{_P2, X2, Y2, N2}|T]) ->
  if
    (N1 /= N2) and (abs(X1-X2) =< ?VISION) and (abs(Y1-Y2) =< ?VISION) ->
      search_enemies({P1, X1,Y1, N1}, T) + 1;
    true -> search_enemies({P1, X1,Y1, N1}, T)
  end.

search_battle({_P1, _X1, _Y1, _N}, []) ->
  {peace, 0};

search_battle({P1, X1, Y1, N1}, [{_P2, X2, Y2, N2}|T]) ->
  if
    (N1 /= N2) and ((X1-X2) =:= 0) and ((Y1-Y2) =:= 0) ->
      io:format("Hay batalla~n"),
      do_battle(N1, N2);
    true ->
      search_battle({P1, X1, Y1, N1}, T)
  end.

do_battle(N1, N2) ->
  Attack1 = random:uniform(10),
  Attack2 = random:uniform(7),
  if
    Attack1 < Attack2 ->
      {player, N2} ! {dead_in_battle, N1},
      {win, N2};
    true ->
      {dead, N2}
  end.

%%====================================================================
%%====================SERVER FUNCTIONS================================
%%====================================================================
init(Players)->
  {ok, Players}.

handle_call(_, _From, {Players}) ->
  {reply, {ok, Players}, {Players}}.

%{room_aceptance, RoomPid}, _State, {Name, Score, _OldRoomPid}
handle_info({add, NodoPlayer, Name}, Players) ->
  io:format("~p entra en la sala junto a ~p ~n", [[{Name,NodoPlayer}], Players]),
  New_Players = Players ++ [{Name, new_position(),new_position(),NodoPlayer}],
  {noreply, New_Players};

handle_info({remove, NodoPlayer}, Players) ->
  io:format("~p se va.", [NodoPlayer]),
  {noreply, Players -- get_player(NodoPlayer,Players)};

handle_info({list_players, a}, Players) ->
  list_players(Players),
  {noreply, Players};

handle_info({move, Player, Move}, Players) ->
  New_Players = move(Player, Move, Players),
  case lists:member(ilegal, New_Players) of
    true ->
      P = get_player(Player, Players),
      {player, Player} ! {results,
          ilegal,
          search_enemies(P,Players),
          search_battle(P,Players)},
      {noreply, Players};
    _ ->
      PN = get_player(Player, New_Players),
      {player, Player} ! {results,
          legal,
          search_enemies(PN, New_Players),
          search_battle(PN, New_Players)},
      {noreply, New_Players}
  end;

handle_info({exit}, Players) ->
  {stop, normal, Players};

handle_info(Info, Estado) ->
  io:format("Generic Info -> ~p~n",[Info]),
  {noreply, Estado}.

handle_cast(_,Estado) ->
  {ok, Estado}.

code_change(_Old, Estado, _) ->
  {ok, Estado}.

terminate(normal, _Estado) ->
  ok.
