%%%-------------------------------------------------------------------
%%% @author Alexander Malev
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 25. aug 2014 21:52
%%%-------------------------------------------------------------------
-module(metatile).
-author("yttrium").

-include_lib("kernel/include/file.hrl").
-include("metatile.hrl").

%% API
-export([xyz_to_meta/4, xyz_to_meta_offset/4, read_meta/1, url2xyz/1, meta_url/1]).
-export([send_xyz/2, send_meta/2, send_tile/3]).


xyz_to_meta(Name, X, Y, Z) ->
  Mask = -?METATILE,
  X0 = X band Mask,
  Y0 = Y band Mask,
  XY = [{X0 bsr 4 * S, Y0 bsr 4 * S} || S <- lists:seq(4, 0, -1)],
  H = [((Xm band 16#0f) bsl 4) bor (Ym band 16#0f) || {Xm, Ym} <- XY],
  ForPath = [?STORAGE, Name, Z | H],
  Path = io_lib:format(<<"~s/~s/~p/~p/~p/~p/~p/~p.meta">>, ForPath),
  list_to_binary(Path).


xyz_to_meta_offset(_Name, X, Y, _Z) ->
  Mask = ?METATILE - 1,
  (X band Mask) * ?METATILE + (Y band Mask).


open(Path) ->
  {ok, _File} = file:open(Path, [raw, binary, read]).

close(File) ->
  file:close(File).

read_meta(Path) when is_binary(Path) ->
  {ok, File} = open(Path),
  MetaTile = read_meta(File),
  close(File),
  MetaTile;
read_meta(File) ->
  {ok, Meta} = file:read(File, 532), % size("META") + 4*int + 2*int*?METATILE*?METATILE
  <<"META", _MM:32/native, X:32/native, Y:32/native, Z:32/native, Tiles/binary>> = Meta,
  ListTiles = [{Offset, Size} || <<Offset:32/native, Size:32/native>> <= Tiles],
  #metatile{x = X, y = Y, z = Z, tiles = ListTiles}.

send_tile(Path, Socket, N) when is_binary(Path) ->
  {ok, File} = open(Path),
  Result = send_tile(File, Socket, N),
  close(File),
  Result;
send_tile(File, Socket, N) when is_record(File, file_descriptor) ->
  #metatile{tiles = ListTiles} = read_meta(File),
  {Offset, Size} = lists:nth(N + 1, ListTiles),
  case gen_tcp:send(Socket, [
    <<"HTTP/1.1 200 OK\n">>,
    <<"Content-Type: image/png\n">>,
    io_lib:format(<<"Content-Length: ~p~n">>, [Size]),
    <<"Server: Erlang-Tile-Server\n\n">>]) of
    ok -> 
      send_file(File, Socket, Offset, Size);
    {error, Reason} ->
      {error, Reason}
  end.

send_file(File, Socket, Offset, Size) ->
  case file:sendfile(File, Socket, Offset, Size, []) of
    {ok, Size} -> {ok, Size};
    {ok, S} -> {size, S};
    {error,closed} -> {error,closed}
  end.

url2xyz(<<"/", Rest/binary>>) ->
  url2xyz(Rest, [<<>>]).
url2xyz(<<".", _/binary>>, YXZName) ->
  [Y, X, Z, Name] = YXZName,
  {Name, X, Y, Z};
url2xyz(<<"/", Rest/binary>>, YXZName) ->
  url2xyz(Rest, [0 | YXZName]);
url2xyz(<<H:8, Rest/binary>>, [Name]) ->
  url2xyz(Rest, [<<Name/binary, H:8>>]);
url2xyz(<<H:8, Rest/binary>>, [N | Tail]) ->
  url2xyz(Rest, [N * 10 + H - $0 | Tail]).

meta_url(Url) ->
  meta_url(Url, <<>>).
meta_url(<<"#", H:8, L:8>>, Url) ->
  {<<?STORAGE/binary, Url/binary, ".meta">>, (H - $0) * 10 + L - $0};
meta_url(<<".", _/binary>>, _) ->
  {badurl, <<"Bad URL">>};
meta_url(<<B:8, Rest/binary>>, Url) ->
  meta_url(Rest, <<Url/binary, B:8>>).

send_xyz(URL, Socket) ->
  {Name, X, Y, Z} = url2xyz(URL),
  Path = xyz_to_meta(Name, X, Y, Z),
  N = xyz_to_meta_offset(Name, X, Y, Z),
  send_tile(Path, Socket, N).

send_meta(URL, Socket) ->
  {Path, N} = meta_url(URL),
  send_tile(Path, Socket, N).
