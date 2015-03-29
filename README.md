# mongodb-erlang-pool [![Build Status](https://travis-ci.org/deadtrickster/mongodb-erlang-pool.svg?branch=master)](https://travis-ci.org/deadtrickster/mongodb-erlang-pool)
Simple Pool for Mongodb-Erlang

###Configuration Example:

```erlang
{mongodb_pool,
  [
   {global_or_local, local},
   {pools, [{test_pool, [
                         {size, 10},
                         {max_overflow, 30}
                        ],[
                           {database, <<"mongodb_pool_test">>},
                           {w_mode, safe}
                          ]}
           ]}

  ]}
```

###Usage
Available shortcuts
`find_one/3`
`find_one/4`
`find_one/5`
`insert/3`
`update/4`
`update/5`
`update/6`
`delete/3`
`delete_one/3`
`count/3`
`count/4`
`command/2`
`ensure_index/3`
```erlang
mongodb_pool:insert(PoolName, Collection, [
                                            {name, <<"Yankees">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"American">>},
                                            {name, <<"Mets">>, home, {city, <<"New York">>, state, <<"NY">>}, league, <<"National">>},
                                            {name, <<"Phillies">>, home, {city, <<"Philadelphia">>, state, <<"PA">>}, league, <<"National">>},
                                            {name, <<"Red Sox">>, home, {city, <<"Boston">>, state, <<"MA">>}, league, <<"American">>}
                                          ])
```
Call `mongodb-erlang` functions:
```erlang
 mongodb_pool:do(PoolName, fun (Connection) ->
                    Cursor = mongo:find(Connection, Collection, Selector, Projector),
                    Result = mc_cursor:rest(Cursor),
                    mc_cursor:close(Cursor),
                    Result
                end).
```
Note how`mongodb_pool:do/1` wraps `find/4` as well as `mc_cursor` calls.
