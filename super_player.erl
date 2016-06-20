-module(super_player).
-behaviour(supervisor).

% Public API
-export([start/1]).
% Internal API
-export([init/1]).

start(Name) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Name).

init(Name) ->
  % estratexia de reinicio de procesos supervisados (one_for_one, one_for_all, rest_for_one)
  RestartStrategy = one_for_one,
  % máxima frecuencia de reinicio (10 reinicios cada 3600 segundos)
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 3600,

  Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  % estratexia de reinicio (permanent: sempre; temporary: nunca; transient: se morre con 'exit')
  Restart = permanent,
  % tempo que permitimos agardar por un proceso supervisado para que execute o seu terminate/2
  Shutdown = 2000,
  % tipo de procesos que imos supervisar
  Type = worker,

  Checker = {player, {player, new, [Name]},
            Restart, Shutdown, Type, [player]},

  {ok, {Flags, [Checker]}}.
