%%%-------------------------------------------------------------------
%%% @author Nikolay Khabarov <2xl@mail.ru>
%%% @copyright (C) 2016 Nikolay Khabarov
%%% @doc cache storage with ets frontend and riak backend
%%% @end
%%%-------------------------------------------------------------------
-module(edis_cache_backend).
-author('Nikolay Khabarov <2xl@mail.ru>').

-behaviour(edis_backend).

-include("edis.hrl").

-record(ref, {riakref    :: edis_riak_backend:ref()}).
-opaque ref() :: #ref{}.
-export_type([ref/0]).

-export([init/3, write/2, put/3, delete/2, fold/3, is_empty/1, destroy/1, status/1, get/2]).

%% ====================================================================
%% Behaviour functions
%% ====================================================================
-spec init(string(), non_neg_integer(), [any()]) -> {ok, ref()} | {error, term()}.
init(Dir, Index, Options) ->
  edis_ets_backend:init(Dir, Index, Options),
  {ok, RiakRef} = edis_riak_backend:init(Dir, Index, Options),
  {ok, #ref{riakref = RiakRef}}.

-spec write(ref(), edis_backend:write_actions()) -> ok | {error, term()}.
write(Ref, Actions) ->
  %% TODO check each operation ret
  [begin
	  case Action of
		  {put, Key, Item} ->
			  put(Ref, Key, Item);
		  {delete, Key} ->
			  delete(Ref, Key);
		  clear ->
			  destroy(Ref)
	  end
   end || Action <- Actions],
  ok.

-spec put(ref(), binary(), #edis_item{}) -> ok | {error, term()}.
put(#ref{riakref = _RiakRef}, _Key, _Item) ->
  ok.

-spec delete(ref(), binary()) -> ok | {error, term()}.
delete(#ref{riakref = _RiakRef}, _Key) ->
  ok.

-spec fold(ref(), edis_backend:fold_fun(), term()) -> term().
fold(#ref{riakref = _RiakRef}, _Fun, _InitValue) ->
  ok.

-spec is_empty(ref()) -> boolean().
is_empty(#ref{riakref = _RiakRef}) ->
  false.

-spec destroy(ref()) -> ok | {error, term()}.
destroy(#ref{riakref = RiakRef}) ->
  edis_ets_backend:destroy(undefined),
  edis_riak_backend:destroy(RiakRef).

-spec status(ref()) -> {ok, binary()} | error.
status(#ref{riakref = _RiakRef}) ->
  {ok, <<"Empty">>}.

-spec get(ref(), binary()) -> #edis_item{} | not_found | {error, term()}.
get(#ref{riakref = _RiakRef}, _Key) ->
  not_found.
