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
-export([start_link/1,
        watch/1,
        unwatch/1,
        show_apps/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER , ?MODULE).
-define(ETS    , restarter).

-record(state  , {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Apps) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Apps], []).

watch({App, Timeout}) ->
  watch({App, Timeout, []});

watch({App, Timeout, Defs}) ->
  maybe_cancel_timer(App),
  Timer = check_after(App, Timeout),
  ets:insert(?ETS, {App, Timeout, Timer, Defs}).

unwatch(App) ->
  maybe_cancel_timer(App),
  ets:delete(?ETS, App).

show_apps() ->
  ets:match_object(?ETS, '_').

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Apps]) ->
  ets:new(?ETS, [set, public, named_table, {keypos, 1}]),
  [appsup:watch(Unit) || Unit <- Apps],
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({check, App}, State) ->
  case ets:lookup(?ETS, App) of
    [{App, Timeout, _, Defs}] ->
      check(App, Defs),
      Timer = check_after(App, Timeout),
      ets:insert(?ETS, {App, Timeout, Timer, Defs});
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
    [{_,_,Timer,_}] ->
      erlang:cancel_timer(Timer);
    _ ->
      ok
  end.

check([], []) ->
  ok;

check([], Defs) ->
  check(Defs, []);

check([App|T], Defs) ->
  case application:ensure_started(App) of
    ok ->
      check(T, Defs);
    {error, {not_started, X}} ->
      check([X,App|T], Defs);
    {error, X} ->
      error_logger:error_report([{"failed restart", App},
                                 {"reason", X}])
  end;

check(App, Defs) ->
  check([App], Defs).
