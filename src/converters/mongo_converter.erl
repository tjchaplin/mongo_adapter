-module(mongo_converter).
-export([json_to_tuple/1,tuple_to_json/1]).
-export([tuple_pairs_to_tuple/1,tuple_to_tuple_pairs/1,tuples_to_tuple_pairs/1]).

json_to_tuple(Value) ->
	JsonTuple = jsx:decode(Value),
	tuple_pairs_to_tuple(JsonTuple,[]).

tuple_pairs_to_tuple(Value) ->
	tuple_pairs_to_tuple(Value,[]).

tuple_pairs_to_tuple([],Acc) ->
	list_to_tuple(Acc);
tuple_pairs_to_tuple([{Key,Value} | Tail], Acc) ->
	AtomKey = if
				is_atom(Key) ->
					Key;
				true ->
					list_to_atom(binary:bin_to_list(Key))
			end,
	ValuePair = to_tuple_value(Value),
	tuple_pairs_to_tuple(Tail, [AtomKey, ValuePair | Acc]).

to_tuple_value([{<<"$oid">>,ObjectIdValue}]) ->
	mongo_types:binary_string_to_objectid(ObjectIdValue);
to_tuple_value([{'$oid',ObjectIdValue}]) ->
	mongo_types:binary_string_to_objectid(ObjectIdValue);
to_tuple_value([Value]) when is_tuple(Value) ->
	tuple_pairs_to_tuple([Value],[]);
to_tuple_value([Value|Tail]) ->
	parse_tuple_values([Value|Tail],[]);
to_tuple_value(Value) ->
	Value.

parse_tuple_values([],Acc) -> 
	lists:reverse(Acc);
parse_tuple_values([Head|Tail],Acc) ->
	Value = case Head of
				[{_Key,_Value}|_PairsTail] ->
					tuple_pairs_to_tuple(Head);
				_ ->
					to_tuple_value(Head)
			end,
	parse_tuple_values(Tail,[Value|Acc]).

tuple_to_json(Value) ->
	Result = tuples_to_tuple_pairs(Value),
	jsx:encode(Result).

tuple_to_tuple_pairs(Value)->
	[Result] = tuples_to_tuple_pairs([Value],[]),
	Result.

tuples_to_tuple_pairs(Value)->
	tuples_to_tuple_pairs(Value,[]).

tuples_to_tuple_pairs([],Acc) -> 
	Acc;
tuples_to_tuple_pairs([Head | Tail], Acc) when is_tuple(Head)->
	TupleList = tuple_to_list(Head),
	TuplePairs = list_to_tuple_pairs(TupleList, []),
	tuples_to_tuple_pairs(Tail, [TuplePairs|Acc]).

list_to_tuple_pairs([],Acc) ->
	Acc;
list_to_tuple_pairs([Key, Value | Tail],Acc) ->
	ValuePair = to_tuple_pair_value(Value),
	list_to_tuple_pairs(Tail,[{Key,ValuePair}|Acc]).

to_tuple_pair_value({Value}) ->
	AsBinaryString = mongo_types:objectid_to_binary_string({Value}),
	[{'$oid',AsBinaryString}];
to_tuple_pair_value(Value) when	is_tuple(Value) ->
	[Value];
to_tuple_pair_value(Value) -> 
	Value.