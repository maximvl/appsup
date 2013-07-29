-module(appsup_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ets:new(restarter, [set, public, named_table, {keypos, 1}]),
  appsup_sup:start_link(),
  case application:get_env(restarts) of
    {ok, List} ->
      [appsup:watch(App, Time) || {App, Time} <- List];
    _ ->
      ok
  end,
  ok.

stop(_State) ->
  ok.
