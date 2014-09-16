%%%-------------------------------------------------------------------
%%% @author Alexander Malev
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 25. aug 2014 21:55
%%%-------------------------------------------------------------------
-module(metatile_tests).
-author("yttrium").

-include_lib("eunit/include/eunit.hrl").
-include("metatile.hrl").

simple_test() ->
  ?assert(true).

xyz_test_() ->
  [test_xyz(), test_xyz_offset()].

test_xyz() ->
  [
    ?_assertEqual(metatile:xyz_to_meta(<<"default">>, 1, 1, 1),
      <<"/var/lib/mod_tile/default/1/0/0/0/0/0.meta">>),
    ?_assertEqual(metatile:xyz_to_meta(<<"default">>, 1, 1, 0),
      <<"/var/lib/mod_tile/default/0/0/0/0/0/0.meta">>),
    ?_assertEqual(metatile:xyz_to_meta(<<"default">>, 1, 0, 1),
      <<"/var/lib/mod_tile/default/1/0/0/0/0/0.meta">>)
  ].

test_xyz_offset() ->
  [
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>, 0, 0, 0), 0),
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>, 1, 1, 0), 9),
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>, 0, 1, 0), 1),
    ?_assertEqual(metatile:xyz_to_meta_offset(<<"default">>, 1, 0, 0), 8)
  ].

meta_read_test_() ->
  {metatile, _, _, _, L} = metatile:read_meta(<<"data/test.meta">>),
  ?_assertEqual(64, length(L)).

url2xyz_test_() ->
  ?_assertEqual({<<"Name">>, 123, 321, 456}, metatile:url2xyz(<<"/Name/123/321/456.png">>)).

meta_url_test_() ->
  ?_assertEqual({<<"/var/lib/mod_tile/default/1/0/0/0/0/0.meta">>, 12},
    metatile:meta_url(<<"/default/1/0/0/0/0/0#12">>)).
