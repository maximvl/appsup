-module(appsup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Arg), {I, {I, start_link, [Arg]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Args = application:get_env(appsup, restarts, []),
  Restarter = ?CHILD(restarter, worker, Args),
  {ok, { {one_for_one, 5, 10}, [Restarter]} }.
