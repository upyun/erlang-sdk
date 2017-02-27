%%%-------------------------------------------------------------------
%% @doc UPYUN Erlang SDK - Utility functions
%%
%% Copyright (c) 2015-present, UPYUN, Inc. All Rights Reserved.
%%
%% This file is released under the MIT license.
%% See the LICENSE for more information.
%%
%%%-------------------------------------------------------------------

-module(upyun_util).

-export([join/2]).
-export([to_binary/1]).
-export([to_integer/1]).
-export([lower_keys/1]).
-export([urlencode/1]).
-export([rfc1123/0, rfc1123/1]).


-spec join(list(), binary()) -> binary().
join([], _) -> <<>>;
join([Item], _) -> Item;
join(Items, Sep) when is_list(Items), is_binary(Sep) ->
    erlang:iolist_to_binary(
        lists:foldr(fun(Item, []) ->
                            [to_binary(Item)];
                       (Item, Acc) ->
                            [to_binary(Item), Sep | Acc]
                    end, [], Items)).

-spec to_binary(any()) -> binary().
to_binary(undefined) ->
    <<>>;
to_binary(Entry) when is_binary(Entry) ->
    Entry;
to_binary(Entry) when is_list(Entry) ->
    erlang:list_to_binary(Entry);
to_binary(Entry) when is_atom(Entry) ->
    erlang:atom_to_binary(Entry, utf8);
to_binary(Entry) when is_integer(Entry) ->
    erlang:integer_to_binary(Entry);
to_binary(Entry) when is_float(Entry) ->
    erlang:iolist_to_binary(io_lib_format:fwrite_g(Entry)).

-spec to_integer(any()) -> integer().
to_integer(I) when is_binary(I) ->
    binary_to_integer(I);
to_integer(I) when is_list(I) ->
    list_to_integer(I);
to_integer(I) -> I.

-spec lower_keys([tuple()]) -> [tuple()].
lower_keys(TupleList) when is_list(TupleList) ->
    lists:foldr(fun({K, V}, Acc) ->
                        [{to_lower(K), V} | Acc]
                end, [], TupleList).

%% @private
-spec to_lower(list() | binary()) -> binary().
to_lower(L) when is_list(L) ->
    to_lower(list_to_binary(L));
to_lower(L) ->
    << << (char_to_lower(C)) >> || << C >> <= L >>.

%% @doc Convert [A-Z] characters to lowercase.
%%      We gain noticeable speed by matching each value directly.
%% @private
-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.

%% @doc Percent-escapes the given binary, and return lowercase output binary
%%      which is required by signature authorization.
-spec urlencode(binary()) -> binary().
urlencode(Url) when is_binary(Url) ->
    hackney_url:pathencode(Url).

-spec rfc1123() -> binary().
rfc1123() ->
    rfc1123(erlang:universaltime()).
-spec rfc1123(calendar:datetime()) -> binary().
rfc1123({{Year, Month, Day}=Date, {Hour, Minute, Second}}) ->
    Weekday = calendar:day_of_the_week(Date),
    << (weekday(Weekday))/binary, ", ",
       (pad_int(Day))/binary, " ",
       (month(Month))/binary, " ",
       (integer_to_binary(Year))/binary, " ",
       (pad_int(Hour))/binary, $:,
       (pad_int(Minute))/binary, $:,
       (pad_int(Second))/binary, " GMT" >>.

%% @doc Following suggestion by MononcQc on #erlounge.
%% @private
-spec pad_int(0..59) -> binary().
pad_int(X) when X < 10 ->
    << $0, ($0 + X) >>;
pad_int(X) ->
    integer_to_binary(X).

%% @private
-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

%% @private
-spec month(1..12) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.
