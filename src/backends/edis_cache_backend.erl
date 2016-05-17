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

-define (CACHETIMEOUT, "cache_ttl_seconds").
-define (DEFAULTCACHETIMEOUT, 2).

-record(ref, {riakref    :: edis_riak_backend:ref(),
              etsref    :: edis_ets_backend:ref()}).
-opaque ref() :: #ref{}.
-export_type([ref/0]).

-type config_option() :: {cache_ttl, non_neg_integer()}.

-export([init/3, write/2, put/3, delete/2, fold/3, is_empty/1, destroy/1, status/1, get/2]).

%% ====================================================================
%% Behaviour functions
%% ====================================================================
-spec init(string(), non_neg_integer(), config_option()) -> {ok, ref()} | {error, term()}.
init(Dir, Index, Options) ->
  CacheTimeOut = proplists:get_value(cache_ttl, Options, ?DEFAULTCACHETIMEOUT),
  erlang:put(?CACHETIMEOUT, CacheTimeOut),
  {ok, EtsRef} = edis_ets_backend:init(Dir, Index, Options),
  {ok, RiakRef} = edis_riak_backend:init(Dir, Index, Options),
  {ok, #ref{riakref = RiakRef, etsref = EtsRef}}.

-spec write(ref(), edis_backend:write_actions()) -> ok | {error, term()}.
write(Ref, [{put, Key, Item} | Actions]) ->
  case put(Ref, Key, Item) of
    ok ->
      write(Ref, Actions);
    Error ->
      Error
  end;
write(Ref, [{delete, Key} | Actions]) ->
  case delete(Ref, Key) of
    ok ->
      write(Ref, Actions);
    Error ->
      Error
  end;
write(Ref, [clear | Actions]) ->
  case destroy(Ref) of
    ok ->
      write(Ref, Actions);
    Error ->
      Error
  end;
write(_Ref, []) ->
  ok.

-spec put(ref(), binary(), #edis_item{}) -> ok | {error, term()}.
put(#ref{riakref = RiakRef, etsref = EtsRef}, Key, Item) ->
  edis_ets_backend:cache(EtsRef, Key, Item, erlang:get(?CACHETIMEOUT)),
  edis_riak_backend:put(RiakRef, Key, Item).

-spec delete(ref(), binary()) -> ok | {error, term()}.
delete(#ref{riakref = RiakRef, etsref = EtsRef}, Key) ->
  edis_ets_backend:delete(EtsRef, Key),
  edis_riak_backend:delete(RiakRef, Key).

-spec fold(ref(), edis_backend:fold_fun(), term()) -> term().
fold(#ref{riakref = RiakRef, etsref = EtsRef}, Fun, InitValue) ->
  edis_ets_backend:fold(EtsRef, Fun, InitValue),
  edis_riak_backend:fold(RiakRef, Fun, InitValue).

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
          edis_ets_backend:cache(EtsRef, Key, Item, erlang:get(?CACHETIMEOUT)),
          Item;
        Result ->
          Result
      end;
    Item when is_record(Item, edis_item) ->
      %% update expire time
      edis_ets_backend:cache(EtsRef, Key, Item, erlang:get(?CACHETIMEOUT)),
      Item;
    Error ->
      Error
  end.
