-module(appsup).

-export([start/0    , stop/0, 
         watch/2    , unwatch/1,
         watch_me/1 , show_apps/0]).

start() ->
  application:start(appsup).

stop() ->
  application:stop(appsup).

watch_me(Timeout) ->
  restarter:watch(application:get_application(), Timeout).

watch(App, Timeout) ->
  restarter:watch(App, Timeout).

unwatch(App) ->
  restarter:unwatch(App).

show_apps() ->
  restarter:show_apps().
