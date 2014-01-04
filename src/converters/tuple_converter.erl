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
	TupleList = tuple_to_list(Head),
	TuplePairs = list_to_tuple_pairs(TupleList, []),
	convert(Tail, [TuplePairs|Acc]).

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