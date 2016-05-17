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

-record(ref, {riakref    :: edis_riak_backend:ref(),
			  etsref    :: edis_ets_backend:ref()}).
-opaque ref() :: #ref{}.
-export_type([ref/0]).

-export([init/3, write/2, put/3, delete/2, fold/3, is_empty/1, destroy/1, status/1, get/2]).

%% ====================================================================
%% Behaviour functions
%% ====================================================================
-spec init(string(), non_neg_integer(), [any()]) -> {ok, ref()} | {error, term()}.
init(Dir, Index, Options) ->
  {ok, EtsRef} = edis_ets_backend:init(Dir, Index, Options),
  {ok, RiakRef} = edis_riak_backend:init(Dir, Index, Options),
  {ok, #ref{riakref = RiakRef, etsref = EtsRef}}.

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
put(#ref{riakref = RiakRef, etsref = _EtsRef}, Key, Item) ->
  edis_riak_backend:put(RiakRef, Key, Item).

-spec delete(ref(), binary()) -> ok | {error, term()}.
delete(#ref{riakref = RiakRef, etsref = EtsRef}, Key) ->
  edis_ets_backend:delete(EtsRef, Key),
  edis_riak_backend:delete(RiakRef, Key).

-spec fold(ref(), edis_backend:fold_fun(), term()) -> term().
fold(#ref{riakref = _RiakRef, etsref = _EtsRef}, _Fun, _InitValue) ->
  %% TODO
  ok.

-spec is_empty(ref()) -> boolean().
is_empty(#ref{riakref = RiakRef, etsref = _EtsRef}) ->
  case edis_ets_backend:is_empty(RiakRef) of
    true ->
      true;
    false ->
      edis_riak_backend:is_empty(RiakRef)
  end.

-spec destroy(ref()) -> ok | {error, term()}.
destroy(#ref{riakref = RiakRef, etsref = EtsRef}) ->
  edis_ets_backend:destroy(EtsRef),
  edis_riak_backend:destroy(RiakRef).

-spec status(ref()) -> {ok, binary()} | error.
status(#ref{riakref = _RiakRef, etsref = _EtsRef}) ->
  {ok, <<"Empty">>}.

-spec get(ref(), binary()) -> #edis_item{} | not_found | {error, term()}.
get(#ref{riakref = RiakRef, etsref = EtsRef}, Key) ->
  case edis_ets_backend:get(EtsRef, Key) of
    not_found ->
      case edis_riak_backend:get(RiakRef, Key) of
        Item when is_record(Item, edis_item) ->
          edis_ets_backend:put(EtsRef, Key, Item),
          Item;
        Result ->
          Result
      end;
    Result ->
      Result
  end.
