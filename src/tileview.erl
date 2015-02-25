%%%-------------------------------------------------------------------
%%% @author Alexander Malev
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 25 feb 2015 22:29
%%%-------------------------------------------------------------------
-module(tileview).
-author("yttrium").

%% API
-export([headers200/2, headers404/1, headers500/1]).

headers200(Socket, Size) ->
  gen_tcp:send(Socket, [
  <<"HTTP/1.1 200 OK\n">>,
  <<"Content-Type: image/png\n">>,
  io_lib:format(<<"Content-Length: ~p~n">>, [Size]),
  <<"Server: Erlang-Tile-Server\n\n">>]).

headers500(Socket) ->
  gen_tcp:send(Socket, [
  <<"HTTP/1.1 500 Internal Server Error\n">>,
  <<"Server: Erlang-Tile-Server\n\n">>]).

headers404(Socket) ->
  gen_tcp:send(Socket, [
  <<"HTTP/1.1 404 Not Found\n">>,
  <<"Server: Erlang-Tile-Server\n\n">>]).
