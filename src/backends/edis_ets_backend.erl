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

-export([init/3, write/2, put/3, delete/2, fold/3, is_empty/1, destroy/1, status/1, get/2]).

-type ref() :: undefined.
-export_type([ref/0]).

%% ====================================================================
%% Behaviour functions
%% ====================================================================
-spec init(string(), non_neg_integer(), [any()]) -> {ok, ref()} | {error, term()}.
init(_Dir, _Index, _Options) ->
  catch (ets:new(?MODULE, [set, public, named_table])),
  {ok, undefined}.

-spec write(ref(), edis_backend:write_actions()) -> ok | {error, term()}.
write(Ref, Actions) ->
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
put(_Ref, Key, Item) ->
  ets:insert(?MODULE, {Key, Item}),
  ok.

-spec delete(ref(), binary()) -> ok | {error, term()}.
delete(_Ref, Key) ->
  ets:delete(?MODULE, Key),
  ok.

-spec fold(ref(), edis_backend:fold_fun(), term()) -> term().
fold(_Ref, Fun, InitValue) ->
  ets:foldl(Fun, InitValue, ?MODULE).

-spec is_empty(ref()) -> boolean().
is_empty(_Ref) ->
  case ets:first(?MODULE) of
    '$end_of_table' ->
           true;
     _Result ->
           false
  end.

-spec destroy(ref()) -> ok | {error, term()}.
destroy(_Ref) ->
  ets:delete_all_objects(?MODULE),
  ok.

-spec status(ref()) -> {ok, binary()} | error.
status(_Ref) ->
  {ok, <<"Empty">>}.

-spec get(ref(), binary()) -> #edis_item{} | not_found | {error, term()}.
get(_Ref, Key) ->
  case ets:lookup(?MODULE, Key) of
    [] ->
      not_found;
    [{_Key, Item}] ->
      Item
  end.
  %#edis_item{key = Key, encoding = raw, type = string, value = <<"Data">>}.
