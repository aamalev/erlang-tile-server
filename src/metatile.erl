%%%-------------------------------------------------------------------
%%% @author yttrium
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. авг 2014 21:52
%%%-------------------------------------------------------------------
-module(metatile).
-author("yttrium").

-define(METATILE,  8).
-define(STORAGE,<<"/var/lib/mod_tile">>).

%% API
-export([xyz_to_meta/4, xyz_to_meta_offset/4]).


xyz_to_meta(Name, X, Y, Z) ->
  Mask = -?METATILE,
  X0 = X band Mask,
  Y0 = Y band Mask,
  XY = [{X0 bsr 4*S, Y0 bsr 4*S} || S <- lists:seq(4, 0, -1)],
  H = [((X band 16#0f) bsl 4) bor (Y band 16#0f) || {X,Y} <- XY],
  ForPath = [?STORAGE|[Name|[Z|H]]],
  Path = io_lib:format(<<"~s/~s/~p/~p/~p/~p/~p/~p.meta">>, ForPath),
  list_to_binary(Path).


xyz_to_meta_offset(Name, X, Y, Z) ->
  Mask = ?METATILE - 1,
  (X band Mask) * ?METATILE + (Y band Mask).
