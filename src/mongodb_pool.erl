-module(mongodb_pool).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include_lib("mongodb/include/mongo_protocol.hrl").

%% API
-export([start/0, stop/0]).
-export([create_pool/4, delete_pool/1]).
-export([
         find_one/3,
         find_one/4,
         find/3,
         find/4,
         insert/3,
         update/4,
         update/5,
         delete/3,
         delete_one/3
        ]).
-export([
         count/3,
         count/4
        ]).
-export([
         command/2,
         ensure_index/3
        ]).
-export([
         do/2
        ]).



%%%===================================================================
%%% API functions (shortcuts)
%%%===================================================================

start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

-spec(create_pool(GlobalOrLocal::atom(), PoolName::atom(), Size::tuple(), Options::[tuple()]) ->
         {ok, pid()} | {error,{already_started, pid()}}).

create_pool(GlobalOrLocal, PoolName, Size, Options) ->
  mongodb_pool_sup:create_pool(GlobalOrLocal, Size, PoolName, Options).

-spec(delete_pool(PoolName::atom()) -> ok | {error,not_found}).
delete_pool(PoolName) ->
  mongodb_pool_sup:delete_pool(PoolName).


%% MongoDB Interface shortcuts

%% @doc Insert a document or multiple documents into a collection.
%%      Returns the document or documents with an auto-generated _id if missing.
-spec insert(term(), mongo:collection(), A) -> A.
insert(PoolName, Coll, Docs) ->
  do(PoolName, fun(Connection) ->
                   mongo:insert(Connection, Coll, Docs)
               end).

%% @doc Replace the document matching criteria entirely with the new Document.
-spec update(term(), mongo:collection(), mongo:selector(), bson:document()) -> ok.
update(PoolName, Coll, Selector, Doc) ->
  do(PoolName, fun(Connection) ->
                   mongo:update(Connection, Coll, Selector, Doc)
               end).

%% @doc Replace the document matching criteria entirely with the new Document.
-spec update(term(), mongo:collection(), mongo:selector(), bson:document(), proplists:proplist()) -> ok.
update(PoolName, Coll, Selector, Doc, Args) ->
  do(PoolName, fun(Connection) ->
                   mongo:update(Connection, Coll, Selector, Doc, Args)
               end).

%% @doc Delete selected documents
-spec delete(term(), mongo:collection(), mongo:selector()) -> ok.
delete(PoolName, Coll, Selector) ->
  do(PoolName, fun(Connection) ->
                   mongo:delete(Connection, Coll, Selector)
               end).

%% @doc Delete first selected document.
-spec delete_one(term(), mongo:collection(), mongo:selector()) -> ok.
delete_one(PoolName, Coll, Selector) ->
  do(PoolName, fun(Connection) ->
                   mongo:delete_one(Connection, Coll, Selector)
               end).

%% @doc Return first selected document, if any
-spec find_one(term(), mongo:collection(), mongo:selector()) -> {} | {bson:document()}.
find_one(PoolName, Coll, Selector) ->
  do(PoolName, fun(Connection) ->
                   mongo:find_one(Connection, Coll, Selector)
               end).

%% @doc Return projection of first selected document, if any. Empty projection [] means full projection.
-spec find_one(term(), mongo:collection(), mongo:selector(), proplists:proplist()) -> {} | {bson:document()}.
find_one(PoolName, Coll, Selector, Args) ->
  do(PoolName, fun(Connection) ->
                   mongo:find_one(Connection, Coll, Selector, Args)
               end).

%% @doc Return selected documents.
-spec find(term(), mongo:collection(), mongo:selector()) -> mongo:cursor().
find(PoolName, Coll, Selector) ->
  do(PoolName, fun(Connection) ->
                   mongo:find(Connection, Coll, Selector)
               end).

%% @doc Return projection of selected documents.
%%      Empty projection [] means full projection.
-spec find(term(), mongo:collection(), mongo:selector(), proplists:proplist()) -> mongo:cursor().
find(PoolName, Coll, Selector, Args) ->
  do(PoolName, fun(Connection) ->
                   mongo:find(Connection, Coll, Selector, Args)
               end).

                                                %@doc Count selected documents
-spec count(term(), mongo:collection(), mongo:selector()) -> integer().
count(PoolName, Coll, Selector) ->
  do(PoolName, fun(Connection) ->
                   mongo:count(Connection, Coll, Selector)
               end).

                                                %@doc Count selected documents up to given max number; 0 means no max.
                                                %     Ie. stops counting when max is reached to save processing time.
-spec count(term(), mongo:collection(), mongo:selector(), integer()) -> integer().
count(PoolName, Coll, Selector, Limit) ->
  do(PoolName, fun(Connection) ->
                   mongo:count(Connection, Coll, Selector, Limit)
               end).

%% @doc Create index on collection according to given spec.
%%      The key specification is a bson documents with the following fields:
%%      key      :: bson document, for e.g. {field, 1, other, -1, location, 2d}, <strong>required</strong>
%%      name     :: bson:utf8()
%%      unique   :: boolean()
%%      dropDups :: boolean()
-spec ensure_index(term(), mongo:collection(), bson:document()) -> ok.
ensure_index(PoolName, Coll, IndexSpec) ->
  do(PoolName, fun(Connection) ->
                   mongo:ensure_index(Connection, Coll, IndexSpec)
               end).

%% @doc Execute given MongoDB command and return its result.
-spec command(term(), bson:document()) -> {boolean(), bson:document()}. % Action
command(PoolName, Command) ->
  do(PoolName, fun(Connection) ->
                   mongo:command(Connection, Command)
               end).

-spec do(term(), mongo:action(A)) -> A.
do(PoolName, Fun) ->
  poolboy:transaction(PoolName, Fun).
