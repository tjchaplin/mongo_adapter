-module(mongo_adapter_integration_tests).

-include_lib("eunit/include/eunit.hrl").

setup(Collection) ->
	application:start(bson),
	application:start(mongodb),
	Connection = [{url,"127.0.0.1"}, {port,27017},{database,'local'}],
	mongo_adapter:start_link(Connection),
	Collection.

teardown(Collection) ->
	mongo_adapter:command({drop, Collection}),
	mongo_adapter:stop(),
	application:stop(mongodb),
	application:stop(bson).

upsert_tuple_test_() ->
	{setup, 
		fun() -> setup('upsert_tuple_test') end, 
		fun teardown/1, 
		fun(SetUpData) ->
			{inparallel,
			[
			 upsert_tuple_string_value(SetUpData),
			 upsert_tuple_array_value(SetUpData),
			 upsert_tuple_int_value(SetUpData),
			 upsert_tuple_inner_object_value(SetUpData),
			 upsert_tuple_inner_object_with_array_value(SetUpData),
			 insert_then_update_tuple_by_object_id(SetUpData),
			 insert_then_update_tuple_by_non_object_id(SetUpData),
			 upsert_then_delete_value(SetUpData)
			 ]}
		end}.

upsert_and_find_value(Collection,ItemToUpsert) ->
	Id=mongo_types:object_id(),
	UpsertValue = [ItemToUpsert,{'_id',Id}],
	mongo_adapter:upsert({Collection, UpsertValue}),
	mongo_adapter:find({Collection,[{'_id',Id}]}).

upsert_tuple_string_value(Collection) ->
	UpsertValue = {key,<<"value1">>},
	[Result] = upsert_and_find_value(Collection,UpsertValue),
	?_assert(lists:any(fun(Item)-> Item == UpsertValue end,Result)).

upsert_tuple_array_value(Collection) ->
	UpsertValue = {key,[<<"value1">>,<<"value2">>]},
	[Result] = upsert_and_find_value(Collection,UpsertValue),
	?_assert(lists:any(fun(Item)-> Item == UpsertValue end,Result)).

upsert_tuple_int_value(Collection) ->
	UpsertValue = {key,1},
	[Result] = upsert_and_find_value(Collection,UpsertValue),
	?_assert(lists:any(fun(Item)-> Item == UpsertValue end,Result)).

upsert_tuple_inner_object_value(Collection) ->
	UpsertValue = {key,[{subKey,<<"subKey1Value1">>}]},
	[Result] = upsert_and_find_value(Collection,UpsertValue),
	?_assert(lists:any(fun(Item)-> Item == UpsertValue end,Result)).

upsert_tuple_inner_object_with_array_value(Collection) ->
	UpsertValue = {key,[{subKey,[<<"subKey1Value1">>,<<"subKey1Value2">>]}]},
	[Result] = upsert_and_find_value(Collection,UpsertValue),
	?_assert(lists:any(fun(Item)-> Item == UpsertValue end,Result)).

insert_then_update_tuple_by_object_id(Collection) ->
	Id=mongo_types:object_id(),
	UpsertValue1 = [{key,<<"value1">>},{'_id',Id}],
	mongo_adapter:upsert({Collection, UpsertValue1}),
	UpsertValue2 = [{key,<<"value2">>},{'_id',Id}],
	mongo_adapter:upsert({Collection, UpsertValue2}),
	[Result]=mongo_adapter:find({Collection,[{'_id',Id}]}),
	Expected = UpsertValue2,
	?_assert(Result == Expected).

insert_then_update_tuple_by_non_object_id(Collection) ->
	Id = <<"UniqueId">>,
	UpsertValue1 = [{key,<<"value1">>},{'_id',Id}],
	mongo_adapter:upsert({Collection, UpsertValue1}),
	UpsertValue2 = [{key,<<"value2">>},{'_id',Id}],
	mongo_adapter:upsert({Collection, UpsertValue2}),
	[Result]=mongo_adapter:find({Collection,[{'_id',Id}]}),
	Expected = UpsertValue2,
	?_assert(Result == Expected).

% insert_then_update_tuple_by_query(Collection) ->
% 	Id=mongo_types:object_id(),
% 	UpsertValue1 = [{key,<<"value1">>},{'_id',Id},{queryableValue,true}],
% 	mongo_adapter:upsert({Collection, UpsertValue1}),
% 	UpsertValue2 = [{key,<<"value2">>},{'_id',Id}],
% 	mongo_adapter:upsert({Collection, UpsertValue2,[{queryableValue,true}]}),
% 	[Result]=mongo_adapter:find({Collection,[{'_id',Id}]}),
% 	Expected = UpsertValue2,
% 	?_assert(Result == Expected).

upsert_then_delete_value(Collection) ->
	Id=mongo_types:object_id(),
	UpsertValue = [{key,<<"value">>},{'_id',Id}],
	mongo_adapter:upsert({Collection, UpsertValue}),
	mongo_adapter:delete({Collection,[{'_id',Id}]}),
	Result=mongo_adapter:find({Collection,[{'_id',Id}]}),
	?_assert(Result==[]).