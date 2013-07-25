%%%-------------------------------------------------------------------
%%% @author maxvel <>
%%% @copyright (C) 2013, maxvel
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2013 by maxvel <>
%%%-------------------------------------------------------------------
-module(restarter).

-behaviour(gen_server).

%% API
-export([start_link/0,
        watch/2,
        unwatch/1,
        show_apps/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER , ?MODULE).
-define(ETS    , restarter).

-record(state  , {}).
-record(unit   , {app, timeout, timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

watch(App, Timeout) ->
  maybe_cancel_timer(App),
  Timer = check_after(App, Timeout),
  ets:insert(?ETS,
             #unit{app=App, timeout=Timeout, timer=Timer}).

unwatch(App) ->
  maybe_cancel_timer(App),
  ets:delete(?ETS, App).

show_apps() ->
  ets:match_object(?ETS, '_').

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?ETS, [set, named_table, {keypos, #unit.app}]),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({check, App}, State) ->
  case application:ensure_started(App) of
    ok ->
      ok;
    {error, X} ->
      error_logger:error_report([{"failed restart", App},
                                 {"reason", X}])
  end,
  case ets:lookup(?ETS, App) of
    [I] ->
      Timer = check_after(App, I#unit.timeout),
      ets:insert(?ETS, I#unit{timer=Timer});
    _ ->
      ok
  end,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_after(atom(), pos_integer()) -> reference().
check_after(App, Timeout) ->
  erlang:send_after(Timeout, ?SERVER, {check, App}).

maybe_cancel_timer(App) ->
  case ets:lookup(?ETS, App) of
    [I] ->
      erlang:cancel_timer(I#unit.timer);
    _ ->
      ok
  end.
