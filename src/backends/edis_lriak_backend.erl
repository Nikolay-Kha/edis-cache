%%%-------------------------------------------------------------------
%%% @author Nikolay Khabarov <2xl@mail.ru>
%%% @copyright (C) 2016 Nikolay Khabarov
%%% @doc riak storage
%%% @end
%%%-------------------------------------------------------------------
-module(edis_lriak_backend).
-author('Nikolay Khabarov <2xl@mail.ru>').

-behaviour(edis_backend).

-include("edis.hrl").

-record(ref, {client    :: any()}).
-opaque ref() :: #ref{}.
-export_type([ref/0]).

-export([init/3, write/2, put/3, delete/2, fold/3, is_empty/1, destroy/1, status/1, get/2]).

%% ====================================================================
%% Behaviour functions
%% ====================================================================
-spec init(string(), non_neg_integer(), any()) -> {ok, ref()} | {error, term()}.
init(_Dir, _Index, _Options) ->
  case riak:local_client() of
    {ok, Client} ->
      {ok, #ref{client = Client}};
    Error ->
      Error
  end.

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
put(#ref{client = Client}, Key, Item) ->
  %% This implementation store edis_item as is. But it is possible
  %% to parse edis_item and store it in a corresponding riak datatype.
  %% 'get' command should be implemented in the same way then.
  {Bucket, RiakKey} = bucketkey(Key),
  Object = case Client:get(Bucket, RiakKey) of
    {ok, ReadObj} ->
      riak_object:update_value(ReadObj, Item);
    _ ->
      riak_object:new(Bucket, RiakKey, Item)
  end,
  Client:put(Object).

-spec delete(ref(), binary()) -> ok | {error, term()}.
delete(#ref{client = Client}, Key) ->
  {Bucket, RiakKey} = bucketkey(Key),
  Client:delete(Bucket, RiakKey).

-spec fold(ref(), edis_backend:fold_fun(), term()) -> term().
fold(#ref{client = _Client}, _Fun, _InitValue) ->
  %% TODO
  %% without it commands DBSIZE, KEYS won't work
  throw(not_implemented).

-spec is_empty(ref()) -> boolean().
is_empty(#ref{client = _Client}) ->
  %% TODO
  false.

-spec destroy(ref()) -> ok | {error, term()}.
destroy(#ref{client = _Client}) ->
  %% TODO cleanup whole db, otherwise flushdb won't work
  ok.

-spec status(ref()) -> {ok, binary()} | error.
status(#ref{client = _client}) ->
  {ok, <<"Empty">>}.

-spec get(ref(), binary()) -> #edis_item{} | not_found | {error, term()}.
get(#ref{client = Client}, Key) ->
  {Bucket, RiakKey} = bucketkey(Key),
  case Client:get(Bucket, RiakKey) of
    {ok, RiakObj} ->
      riak_object:get_value(RiakObj);
    {error, notfound} ->
      not_found;
    Error ->
      Error
  end.

%% ====================================================================
%% Private functions
%% ====================================================================
-spec bucketkey(binary()) -> {binary(), binary()}.
bucketkey(Str) ->
  DefaultBucket = <<"default">>,
  case binary:split(Str, <<$:>>, []) of
    [Key] ->
      {DefaultBucket, Key};
    [Key, <<>>] ->
      {DefaultBucket, Key};
    [<<>>, Key] ->
      {DefaultBucket, Key};
    [Bucket, Key] ->
      {Bucket, Key}
  end.
