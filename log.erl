-module(log).
-behaviour(gen_event).

%% Internal API (gen_event)
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

%% ===== ===== ===== ===== ===== ===== ===== ===== =====
init(Element) ->
  {ok, Fd} = file:open(Element, [read, append]),
  {ok, Fd}.

handle_event({save_log, {Content}}, Fd) ->
  {_, Time} = calendar:gregorian_seconds_to_datetime(erlang:system_time(seconds)),
  io:format(Fd, "[~p]: ~p~n", [Time, Content]),
  {ok, Fd}.

handle_call(_Call, Estado) ->
    {ok, invalid, Estado}.

handle_info(_Info, Estado) ->
    {ok, Estado}.

code_change(_Old, Estado, _) ->
    {ok, Estado}.

terminate([], Fd) ->
    file:close(Fd).

%% ===== ===== ===== ===== ===== ===== ===== ===== =====
