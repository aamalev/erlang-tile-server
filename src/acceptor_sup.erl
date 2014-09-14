%%%-------------------------------------------------------------------
%%% @author Alexander Malev
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 22. авг 2014 22:22
%%%-------------------------------------------------------------------
-module(acceptor_sup).
-author("yttrium").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Opts :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Opts) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init(Opts) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  [Port|_] = [Port || {port, Port} <- Opts],
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet,http_bin}]),

  [Acceptors|_] = [Acceptors || {acceptors, Acceptors} <- Opts],
  spawn(fun() -> add_children(Acceptors) end),

  AcceptorChild = {acceptor, {acceptor, start_link, [ListenSocket]},
    Restart, Shutdown, Type, [acceptor]},

  {ok, {SupFlags, [AcceptorChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_children(Acceptors) ->
  [supervisor:start_child(?MODULE,[]) || _ <- lists:seq(1,Acceptors)],
  ok.
