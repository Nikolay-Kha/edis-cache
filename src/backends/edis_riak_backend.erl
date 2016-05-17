%%%-------------------------------------------------------------------
%%% @author Nikolay Khabarov <2xl@mail.ru>
%%% @copyright (C) 2016 Nikolay Khabarov
%%% @doc riak storage
%%% @end
%%%-------------------------------------------------------------------
-module(edis_riak_backend).
-author('Nikolay Khabarov <2xl@mail.ru>').

-behaviour(edis_backend).

-include("edis.hrl").
-include_lib("riakc/include/riakc.hrl").

-record(ref, {pid    :: pid()}).
-opaque ref() :: #ref{}.
-export_type([ref/0]).

-type config_option() :: {riak_host, string() | atom() | inet:ip_address()}
                       | {riak_port, non_neg_integer()}.

-export([init/3, write/2, put/3, delete/2, fold/3, is_empty/1, destroy/1, status/1, get/2]).

%% ====================================================================
%% Private functions
%% ====================================================================
-spec bucketkey(binary()) -> {bucket(), key()}.
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

%% ====================================================================
%% Behaviour functions
%% ====================================================================
-spec init(string(), non_neg_integer(), config_option()) -> {ok, ref()} | {error, term()}.
init(_Dir, _Index, Options) ->
  Host = proplists:get_value(riak_host, Options),
  Port = proplists:get_value(riak_port, Options),
  case riakc_pb_socket:start_link(Host, Port) of
    {ok, Pid} ->
      {ok, #ref{pid = Pid}};
    Error ->
      Error
  end.

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
put(#ref{pid = Pid}, Key, Item) ->
  {Bucket, RiakKey} = bucketkey(Key),
  Object = case riakc_pb_socket:get(Pid, Bucket, RiakKey) of
    {ok, ReadObj} ->
      riakc_obj:update_value(ReadObj, Item);
    _ ->
      riakc_obj:new(Bucket, RiakKey, Item)
  end,
  riakc_pb_socket:put(Pid, Object).

-spec delete(ref(), binary()) -> ok | {error, term()}.
delete(#ref{pid = Pid}, Key) ->
  {Bucket, RiakKey} = bucketkey(Key),
  riakc_pb_socket:delete(Pid, Bucket, RiakKey).

-spec fold(ref(), edis_backend:fold_fun(), term()) -> term().
fold(#ref{pid = _Pid}, _Fun, _InitValue) ->
  ok.

-spec is_empty(ref()) -> boolean().
is_empty(#ref{pid = _Pid}) ->
  %% TODO
  false.

-spec destroy(ref()) -> ok | {error, term()}.
destroy(#ref{pid = Pid}) ->
  riakc_pb_socket:stop(Pid).

-spec status(ref()) -> {ok, binary()} | error.
status(#ref{pid = _Pid}) ->
  {ok, <<"Empty">>}.

-spec get(ref(), binary()) -> #edis_item{} | not_found | {error, term()}.
get(#ref{pid = Pid}, Key) ->
  {Bucket, RiakKey} = bucketkey(Key),
  case riakc_pb_socket:get(Pid, Bucket, RiakKey) of
    {ok, RiakObj} ->
      binary_to_term(riakc_obj:get_value(RiakObj));
    {error, notfound} ->
      not_found;
    Error ->
      Error
  end.
