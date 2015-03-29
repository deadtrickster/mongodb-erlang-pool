-module(mongodb_pool_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         end_per_testcase/2
        ]).

-export([
         insert_and_find/1,
         insert_and_delete/1,
         search_and_query/1,
         update/1,
         sort_and_limit/1
        ]).

all() ->
  [
   insert_and_find,
   insert_and_delete,
   search_and_query,
   update,
   sort_and_limit
  ].

-define(POOL, test_pool).
-define(COLLECTION, <<"test">>).

init_per_suite(Config) ->
  application:start(bson),
  application:start(mongodb),
  application:start(mongodb_pool),
  Config.

end_per_suite(_Config) ->
  application:stop(mongodb),
  application:stop(bson),
  application:stop(mongodb_pool),
  ok.

end_per_testcase(_Case, _Config) ->
  mongodb_pool:delete(?POOL, ?COLLECTION, {}).

insert_and_find(_Config) ->

  Teams = mongodb_pool:insert(?POOL, ?COLLECTION, [
                                                {name, <<"Yankees">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"American">>},
                                                {name, <<"Mets">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"National">>},
                                                {name, <<"Phillies">>, home, {city, <<"Philadelphia">>, state, <<"PA">>}, league, <<"National">>},
                                                {name, <<"Red Sox">>, home, {city, <<"Boston">>, state, <<"MA">>}, league, <<"American">>}
                                               ]),
  4 = mongodb_pool:count(?POOL, ?COLLECTION, {}),
  Teams2 = find(?POOL, ?COLLECTION, {}),
  true = match_bson(Teams, Teams2),

  NationalTeams = [Team || Team <- Teams, bson:at(league, Team) == <<"National">>],
  NationalTeams2 = find(?POOL, ?COLLECTION, {league, <<"National">>}),
  true = match_bson(NationalTeams, NationalTeams2),

  2 = mongodb_pool:count(?POOL, ?COLLECTION, {league, <<"National">>}),

  TeamNames = [bson:include([name], Team) || Team <- Teams],
  TeamNames = find(?POOL, ?COLLECTION, {}, {'_id', 0, name, 1}),


  BostonTeam = lists:last(Teams),
  {BostonTeam2} = mongodb_pool:find_one(?POOL, ?COLLECTION, {home, {city, <<"Boston">>, state, <<"MA">>}}),
  true = match_bson([BostonTeam], [BostonTeam2]).

insert_and_delete(_Config) ->
  mongodb_pool:insert(?POOL, ?COLLECTION, [
    {name, <<"Yankees">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"American">>},
    {name, <<"Mets">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"National">>},
    {name, <<"Phillies">>, home, {city, <<"Philadelphia">>, state, <<"PA">>}, league, <<"National">>},
    {name, <<"Red Sox">>, home, {city, <<"Boston">>, state, <<"MA">>}, league, <<"American">>}
  ]),
  4 = mongodb_pool:count(?POOL, ?COLLECTION, {}),

  mongodb_pool:delete_one(?POOL, ?COLLECTION, {}),
  3 = mongodb_pool:count(?POOL, ?COLLECTION, {}).

search_and_query(Config) ->

  %insert test data
  mongodb_pool:insert(?POOL, ?COLLECTION, [
    Yankees = {name, <<"Yankees">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"American">>},
    Mets = {name, <<"Mets">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"National">>},
    {name, <<"Phillies">>, home, {city, <<"Philadelphia">>, state, <<"PA">>}, league, <<"National">>},
    {name, <<"Red Sox">>, home, {city, <<"Boston">>, state, <<"MA">>}, league, <<"American">>}
  ]),

  %test selector
  Res = find(?POOL, ?COLLECTION, {home, {city, <<"New York">>, state, <<"NY">>}}),
  ct:log("Got ~p", [Res]),
  [YankeesBSON, MetsBSON] = Res,
  true = is_equal_bsons(Yankees, YankeesBSON),
  true = is_equal_bsons(Mets, MetsBSON),

  %test projector
  Res2 = find(?POOL, ?COLLECTION, {home, {city, <<"New York">>, state, <<"NY">>}}, {name, true, league, true}),
  ct:log("Got ~p", [Res2]),
  [YankeesProjectedBSON, MetsProjectedBSON] = Res2,
  true = match_bson({name, <<"Yankees">>, league, <<"American">>}, YankeesProjectedBSON),
  true = match_bson({name, <<"Mets">>,league, <<"National">>}, MetsProjectedBSON),
  Config.

sort_and_limit(Config) ->

  %insert test data
  mongodb_pool:insert(?POOL, ?COLLECTION, [
    {key, <<"test">>, value, <<"two">>, tag, 2},
    {key, <<"test">>, value, <<"one">>, tag, 1},
    {key, <<"test">>, value, <<"four">>, tag, 4},
    {key, <<"another">>, value, <<"five">>, tag, 5},
    {key, <<"test">>, value, <<"three">>, tag, 3}
  ]),

  %test match and sort
  {true, {result, Res}} = mongodb_pool:command(?POOL, {aggregate, ?COLLECTION, pipeline, [{'$match', {key, <<"test">>}}, {'$sort', {tag, 1}}]}),
  ct:log("Got ~p", [Res]),

  true = is_equal_lists_of_bsons(
           [
            {key, <<"test">>, value, <<"one">>, tag, 1},
            {key, <<"test">>, value, <<"two">>, tag, 2},
            {key, <<"test">>, value, <<"three">>, tag, 3},
            {key, <<"test">>, value, <<"four">>, tag, 4}
           ],
           Res),

  %test match & sort with limit
  {true, {result, Res1}} = mongodb_pool:command(?POOL,
    {aggregate, ?COLLECTION, pipeline,
      [
        {'$match', {key, <<"test">>}},
        {'$sort', {tag, 1}},
        {'$limit', 1}
      ]}),
  ct:log("Got ~p", [Res1]),
  true = is_equal_lists_of_bsons(
           [
            {key, <<"test">>, value, <<"one">>, tag, 1}
           ],
           Res1),
  Config.

update(Config) ->

  %insert test data
  mongodb_pool:insert(?POOL, ?COLLECTION,
    {'_id', 100,
      sku, <<"abc123">>,
      quantity, 250,
      instock, true,
      reorder, false,
      details, {model, "14Q2", make, "xyz"},
      tags, ["apparel", "clothing"],
      ratings, [{by, "ijk", rating, 4}]}
  ),

  %check data inserted
  Res = find(?POOL, ?COLLECTION, {'_id', 100}),
  ct:log("Got ~p", [Res]),
  [{'_id', 100,
    sku, <<"abc123">>,
    quantity, 250,
    instock, true,
    reorder, false,
    details, {model, "14Q2", make, "xyz"},
    tags, ["apparel", "clothing"],
    ratings, [{by, "ijk", rating, 4}]}] = Res,

  %update existent fields
  Command = {'$set', {
    quantity, 500,
    details, {model, "14Q3", make, "xyz"},
    tags, ["coats", "outerwear", "clothing"]
  }},
  mongodb_pool:update(?POOL, ?COLLECTION, {'_id', 100}, Command),

  %check data updated
  [Res1] = find(?POOL, ?COLLECTION, {'_id', 100}),
  ct:log("Got ~p", [Res1]),
  true = is_equal_bsons(
           Res1,
           {'_id', 100,
            sku, <<"abc123">>,
            quantity, 500,
            instock, true,
            reorder, false,
            details, {model, "14Q3", make, "xyz"},
            tags, ["coats", "outerwear", "clothing"],
            ratings, [{by, "ijk", rating, 4}]}),

  %update non existent fields
  Command1 = {'$set', {expired, true}},
  mongodb_pool:update(?POOL, ?COLLECTION, {'_id', 100}, Command1),

  %check data updated
  [Res2] = find(?POOL, ?COLLECTION, {'_id', 100}),
  ct:log("Got ~p", [Res2]),
  true = is_equal_bsons(
           Res2,
           {'_id', 100,
            sku, <<"abc123">>,
            quantity, 500,
            instock, true,
            reorder, false,
            details, {model, "14Q3", make, "xyz"},
            tags, ["coats", "outerwear", "clothing"],
            ratings, [{by, "ijk", rating, 4}],
            expired, true}),

  %update embedded fields
  Command2 = {'$set', {'details.make', "zzz"}}, %TODO make bson avoid using atoms
  mongodb_pool:update(?POOL, ?COLLECTION, {'_id', 100}, Command2),

  %check data updated
  [Res3] = find(?POOL, ?COLLECTION, {'_id', 100}),
  ct:log("Got ~p", [Res3]),
  true = is_equal_bsons(
           Res3,
           {'_id', 100,
            sku, <<"abc123">>,
            quantity, 500,
            instock, true,
            reorder, false,
            details, {model, "14Q3", make, "zzz"},
            tags, ["coats", "outerwear", "clothing"],
            ratings, [{by, "ijk", rating, 4}],
            expired, true}),

  %update list elements
  Command3 = {'$set', { %TODO make bson avoid using atoms
    'tags.1', "rain gear",
    'ratings.0.rating', 2
  }},
  mongodb_pool:update(?POOL, ?COLLECTION, {'_id', 100}, Command3),
  [Res4] = find(?POOL, ?COLLECTION, {'_id', 100}),
  ct:log("Got ~p", [Res4]),
  true = is_equal_bsons(
           Res4,
           {'_id', 100,
            sku, <<"abc123">>,
            quantity, 500,
            instock, true,
            reorder, false,
            details, {model, "14Q3", make, "zzz"},
            tags, ["coats", "rain gear", "clothing"],
            ratings, [{by, "ijk", rating, 2}],
            expired, true}),
  Config.


%% @private

find(PoolName, Collection, Selector) ->
  find(PoolName, Collection, Selector, []).

find(PoolName, Collection, Selector, Projector) ->
  mongodb_pool:do(PoolName, fun (Connection) ->
                    Cursor = mongo:find(Connection, Collection, Selector, Projector),
                    Result = mc_cursor:rest(Cursor),
                    mc_cursor:close(Cursor),
                    Result
                end).

%% collection(Case) ->
%%   Now = now_to_seconds(erlang:now()),
%%   <<(atom_to_binary(?MODULE, utf8))/binary, $-,
%%   (atom_to_binary(Case, utf8))/binary, $-,
%%   (list_to_binary(integer_to_list(Now)))/binary>>.

%% @private
%% now_to_seconds({Mega, Sec, _}) ->
%%   (Mega * 1000000) + Sec.

match_bson(Tuple1, Tuple2) when length(Tuple1) /= length(Tuple2) -> false;
match_bson(Tuple1, Tuple2) ->
  try
    lists:foldr(
      fun(Elem, Num) ->
        Elem2 = lists:nth(Num, Tuple2),
        Sorted = lists:sort(bson:fields(Elem)),
        Sorted = lists:sort(bson:fields(Elem2))
      end, 1, Tuple1)
  catch
    _:_ -> false
  end,
  true.

is_equal_bsons(LHV, RHV) ->
    LHVSorted = sort_bson_data(LHV),
    LHVSorted =:= sort_bson_data(RHV).

is_equal_lists_of_bsons(LHV, RHV) ->
    LHVSorted = lists:sort([sort_bson_data(BSON) || BSON <- LHV]),
    LHVSorted =:= lists:sort([sort_bson_data(BSON) || BSON <- RHV]).

sort_bson_data(BSON) ->
    lists:keydelete('_id', 1, lists:sort(bson:fields(BSON))).
