%%%-------------------------------------------------------------------
%%% @author Alexander Malev
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 14. sep 2014 15:45
%%%-------------------------------------------------------------------
-author("yttrium").

-define(METATILE,  8).
-define(STORAGE,<<"/var/lib/mod_tile">>).

-record(metatile,{x,y,z,tiles}).
