-module(appsup).

-export([start/0, stop/0, watch/2, unwatch/1, show_apps/0]).

start() ->
  application:start(appsup).

stop() ->
  application:stop(appsup).

watch(App, Timeout) ->
  restarter:watch(App, Timeout).

unwatch(App) ->
  restarter:unwatch(App).

show_apps() ->
  restarter:show_apps().
