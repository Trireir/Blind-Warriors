
-module(server).

-export([start/0, stop/0, list_rooms/0, list_rooms/1, list_players/0, list_players/1, loop/1, find_name/1, get_room/1, ocupy_room/2, liberate_room/2,new_room/1]).
-import(room, [start_room/1]).

-define(ROOM_SIZE, 4).
-define(ROOM, room).

-define(LOG, loggen).
-define(LOG_MODULE, log).
-define(LOG_FILE, "server_log.txt").

%%--------------------------------------------------------------------
%% @doc Start the central server
%% @end
%% -------------------------------------------------------------------
-spec start() -> ok.
start() ->
  io:format("Encendemos servidor central..."),
  register(server, spawn(?MODULE, loop, [[]])),
  ok.

%%--------------------------------------------------------------------
%% @doc Stop the central server
%% @end
%% -------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
  ?MODULE ! stop,
  unregister(server).

%%--------------------------------------------------------------------
%% @doc Print the operative rooms
%% @end
%% -------------------------------------------------------------------
-spec list_rooms() -> ok.
list_rooms() ->
  ?MODULE ! list.

list_rooms([]) ->
  ok;
list_rooms([H|T]) ->
  io:format("~p~n", [H]),
  list_rooms(T).

%%--------------------------------------------------------------------
%% @doc Print the players in game
%% @end
%% -------------------------------------------------------------------
-spec list_players() -> ok.
list_players() ->
  ?MODULE ! list_players.

list_players([]) ->
  ok;
list_players([{Pid, Ocuppacy}|T]) ->
  io:format("Sala ~p ~p~n =========== ~n", [Pid, Ocuppacy]),
  Pid ! {list_players, a},
  list_players(T).

%%--------------------------------------------------------------------
%% @doc  If there are space in one rome, return the PidRoom
% If not, return the atom new_room
%% @end
%% -------------------------------------------------------------------
-spec get_room(ListPlayers:: list()) -> ok.
get_room([]) ->
  new_room;
get_room([{PidRoom, Ocuppacy}|T]) ->
  if
    Ocuppacy < ?ROOM_SIZE ->
      PidRoom;
    true ->
      get_room(T)
  end.

find_name(N) ->
  list_to_atom(lists:flatten(io_lib:format("room~p", [N]))).

new_room(Name) ->
  start_room(Name).

% Add one to the Room
% ocupy_room(Room, Rooms)
ocupy_room(_Room, []) ->
  error_ocupy_room;
ocupy_room(Room, [{PidR, O} | T]) ->
  if
    Room == PidR ->
      [{PidR, O+1}|T];
    true ->
      [{PidR, O}|ocupy_room(Room, T)]
  end.

% Add one to the Room
% ocupy_room(Room, Rooms)
liberate_room(_Room, []) ->
  error_liberate_room;
liberate_room(Room, [{PidR, O} | T]) ->
  if
    Room == PidR ->
      if O -1 == 0 ->
        io:format("Cerramos la sala."),
        PidR ! {exit},
        T;
      true ->
        [{PidR, O-1}|T]
      end;
    true ->
      [{PidR, O}|liberate_room(Room, T)]
  end.

% Players [{NodoRoom, Ocuppacy}]
loop(Rooms) ->
  receive
    {enter_room, NodoPlayer, Name} ->
      io:format("Buscando sala para ~p~n",[NodoPlayer]),
      R = get_room(Rooms),
      case R of
        new_room ->
          NR = new_room(find_name(length(Rooms))),
          NR ! {add, NodoPlayer, Name},
          {player, NodoPlayer} ! {room_aceptance, NR},
          loop(Rooms ++ [{NR, 1}]);
        _ ->
          R ! {add, NodoPlayer, Name},
          {player, NodoPlayer} ! {room_aceptance, R},
          loop(ocupy_room(R, Rooms))
      end;

    {exit_room, RoomPid} ->
      io:format("~p pierde un jugador.", [RoomPid]),
      loop(liberate_room(RoomPid, Rooms));

    list ->
      list_rooms(Rooms),
      loop(Rooms);

    list_players ->
      list_players(Rooms),
      loop(Rooms);
    stop ->
      io:fwrite("Server Stopped.~n")
  end.
