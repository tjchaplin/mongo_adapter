-module(tuple_converter).
-export([convert/1,convert_single/1]).

convert_single(Value)->
	[Result] = convert([Value],[]),
	Result.

convert(Value)->
	convert(Value,[]).

convert([],Acc) -> 
	Acc;
convert([Head | Tail], Acc) when is_tuple(Head)->
	TuplePairs = to_tuple_pairs(Head),
	convert(Tail, [TuplePairs|Acc]).

to_tuple_pairs(Tuple) when is_tuple(Tuple) ->
	TupleList = tuple_to_list(Tuple),
	TuplePairs = list_to_tuple_pairs(TupleList, []),
	TuplePairs.

list_to_tuple_pairs([],Acc) ->
	Acc;
list_to_tuple_pairs([Key, Value | Tail],Acc) ->
	ValuePair = to_tuple_pair_value(Value),
	list_to_tuple_pairs(Tail,[{Key,ValuePair}|Acc]).

to_tuple_pair_value({Value}) ->
	AsBinaryString = mongo_types:objectid_to_binary_string({Value}),
	[{'$oid',AsBinaryString}];
to_tuple_pair_value(Value) when	is_tuple(Value)->
	case tuple_size(Value) of
		2 ->
			[Value];
		_ ->
			TuplePairs = to_tuple_pairs(Value),
			TuplePairs
	end;

to_tuple_pair_value(Value) -> 
	Value.