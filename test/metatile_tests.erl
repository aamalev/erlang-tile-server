%%%-------------------------------------------------------------------
%%% @author yttrium
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. авг 2014 21:55
%%%-------------------------------------------------------------------
-module(metatile_tests).
-author("yttrium").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

xyz_test_() ->
  [test_xyz(),test_xyz_offset()].

test_xyz() ->
  [
    ?_assertEqual(metatile:xyz_to_meta(<<"default">>, 1,1,1),
      <<"/var/lib/mod_tile/default/1/0/0/0/0/0.meta">>),
    ?_assertEqual(metatile:xyz_to_meta(<<"default">>, 1,1,0),
      <<"/var/lib/mod_tile/default/0/0/0/0/0/0.meta">>),
    ?_assertEqual(metatile:xyz_to_meta(<<"default">>, 1,0,1),
      <<"/var/lib/mod_tile/default/1/0/0/0/0/0.meta">>)
  ].

test_xyz_offset() ->
  [
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>,0,0,0),0),
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>,1,1,0),9),
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>,0,1,0),1),
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>,1,0,0),8)
  ].
