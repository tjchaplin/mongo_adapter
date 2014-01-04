-module(tuple_pairs_converter).
-export([convert/1]).

convert(Value) ->
	convert(Value,[]).

convert([],Acc) ->
	list_to_tuple(Acc);
convert([{Key,Value} | Tail], Acc) ->
	AtomKey = if
				is_atom(Key) ->
					Key;
				true ->
					list_to_atom(binary:bin_to_list(Key))
			end,
	ValuePair = to_tuple_value(Value),
	convert(Tail, [AtomKey, ValuePair | Acc]).

to_tuple_value([{<<"$oid">>,ObjectIdValue}]) ->
	mongo_types:binary_string_to_objectid(ObjectIdValue);
to_tuple_value([{'$oid',ObjectIdValue}]) ->
	mongo_types:binary_string_to_objectid(ObjectIdValue);
to_tuple_value([Value]) when is_tuple(Value) ->
	convert([Value],[]);
to_tuple_value([Value|Tail]) ->
	parse_tuple_values([Value|Tail],[]);
to_tuple_value(Value) ->
	Value.

parse_tuple_values([],Acc) -> 
	lists:reverse(Acc);
parse_tuple_values([Head|Tail],Acc) ->
	Value = case Head of
				[{_Key,_Value}|_PairsTail] ->
					convert(Head);
				_ ->
					to_tuple_value(Head)
			end,
	parse_tuple_values(Tail,[Value|Acc]).