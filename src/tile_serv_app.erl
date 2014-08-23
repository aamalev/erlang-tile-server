-module(tile_serv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tile_serv_sup:start_link([{port, 8090}, {acceptors, 20}]).

stop(_State) ->
    ok.
