-module(appsup).

-export([start/0    , stop/0, 
         watch/2    , unwatch/1,
         watch_me/1 , unwatch_me/0,
         show_apps/0]).

start() ->
  application:start(appsup).

stop() ->
  application:stop(appsup).

watch_me(Timeout) ->
  case application:get_application() of
    {ok, App} ->
      restarter:watch(App, Timeout);
    _ ->
      {error, bad_application}
  end.

watch(App, Timeout) ->
  restarter:watch(App, Timeout).

unwatch(App) ->
  restarter:unwatch(App).

unwatch_me() ->
  case application:get_application() of
    {ok, App} ->
      restarter:unwatch(App);
    _ ->
      {error, bad_application}
  end.

show_apps() ->
  restarter:show_apps().
