%%%-------------------------------------------------------------------
%%% @author Nikolay Khabarov <2xl@mail.ru>
%%% @copyright (C) 2016 Nikolay Khabarov
%%% @doc ets storage
%%% @end
%%%-------------------------------------------------------------------
-module(edis_ets_backend).
-author('Nikolay Khabarov <2xl@mail.ru>').

-behaviour(edis_backend).

-include("edis.hrl").

-record(ref, {}).
-opaque ref() :: #ref{}.
-export_type([ref/0]).

-export([init/3, write/2, put/3, delete/2, fold/3, is_empty/1, destroy/1, status/1, get/2]).

%% ====================================================================
%% Behaviour functions
%% ====================================================================
-spec init(string(), non_neg_integer(), [any()]) -> {ok, ref()} | {error, term()}.
init(_Dir, _Index, _Options) ->
  {ok, #ref{}}.

-spec write(ref(), edis_backend:write_actions()) -> ok | {error, term()}.
write(#ref{}, Actions) ->
  ok.

-spec put(ref(), binary(), #edis_item{}) -> ok | {error, term()}.
put(#ref{}, Key, Item) ->
  ok.

-spec delete(ref(), binary()) -> ok | {error, term()}.
delete(#ref{}, Key) ->
  ok.

-spec fold(ref(), edis_backend:fold_fun(), term()) -> term().
fold(#ref{}, Fun, InitValue) ->
  ok.

-spec is_empty(ref()) -> boolean().
is_empty(#ref{}) ->
  ok.

-spec destroy(ref()) -> ok | {error, term()}.
destroy(#ref{}) ->
  ok.

-spec status(ref()) -> {ok, binary()} | error.
status(#ref{}) ->
  {ok, <<"Empty">>}.

-spec get(ref(), binary()) -> #edis_item{} | not_found | {error, term()}.
get(#ref{}, Key) ->
  #edis_item{key = Key, encoding = raw, type = string, value = <<"Data">>}.
