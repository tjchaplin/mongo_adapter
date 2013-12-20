-module(mongo_converter_tests).

-include_lib("eunit/include/eunit.hrl").

when_tuple_pair_with_atom_single_value_test() -> 
	TestValue = [{key,<<"value1">>}],
	Expected = {key,<<"value1">>},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_pair_with_binary_string_single_value_test() -> 
	TestValue = [{<<"key">>,<<"value1">>}],
	Expected = {key,<<"value1">>},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_with_single_value_test() -> 
	TestValue = {key,<<"value1">>},
	Expected = [{key,<<"value1">>}],
	Result = mongo_converter:tuple_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_single_value_test() -> 
	TestValue = [{key,<<"value1">>}],
	Expected = [{key,<<"value1">>}],
	Result = mongo_converter:tuples_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_tuple_pair_with_atom_multiple_values_test() -> 
	TestValue = [{'_id',1},{key,<<"value1">>}],
	Expected = {key,<<"value1">>,'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_pair_with_binary_string_multiple_values_test() -> 
	TestValue = [{<<"_id">>,1},{<<"key">>,<<"value1">>}],
	Expected = {key,<<"value1">>,'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_with_multiple_values_test() -> 
	TestValue ={'_id',1,key,<<"value1">>},
	Expected = [{key,<<"value1">>},{'_id',1}],
	Result = mongo_converter:tuple_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_multiple_values_test() -> 
	TestValue =[{'_id',1,key,<<"value1">>}],
	Expected = [{key,<<"value1">>},{'_id',1}],
	Result = mongo_converter:tuples_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_tuple_pair_with_atom_keys_is_array_of_strings_test() -> 
	TestValue = [{'_id',1},{key,[<<"value1">>,<<"value2">>]}],
	Expected = {key,[<<"value1">>,<<"value2">>],'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_pair_with_binary_string_keys_is_array_of_strings_test() -> 
	TestValue = [{<<"_id">>,1},{<<"key">>,[<<"value1">>,<<"value2">>]}],
	Expected = {key,[<<"value1">>,<<"value2">>],'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_is_array_of_strings_test() -> 
	TestValue = {'_id',1,key,[<<"value1">>,<<"value2">>]},
	Expected = [{key,[<<"value1">>,<<"value2">>]},{'_id',1}],
	Result = mongo_converter:tuple_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_list_tuple_is_array_of_strings_test() -> 
	TestValue = [{'_id',1,key,[<<"value1">>,<<"value2">>]}],
	Expected = [{key,[<<"value1">>,<<"value2">>]},{'_id',1}],
	Result = mongo_converter:tuples_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_tuple_pair_with_atom_keys_is_array_of_ints_test() -> 
	TestValue = [{'_id',1},{key,[1,2]}],
	Expected = {key,[1,2],'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_pair_with_binary_string_keys_is_array_of_ints_test() -> 
	TestValue = [{<<"_id">>,1},{<<"key">>,[1,2]}],
	Expected = {key,[1,2],'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_is_array_of_ints_test() -> 
	TestValue = {'_id',1,key,[1,2]},
	Expected = [{key,[1,2]},{'_id',1}],
	Result = mongo_converter:tuple_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_list_tuple_is_array_of_ints_test() -> 
	TestValue = [{'_id',1,key,[1,2]}],
	Expected = [{key,[1,2]},{'_id',1}],
	Result = mongo_converter:tuples_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_tuple_pairs_with_atom_keys_has_sub_tuple_test() -> 
	TestValue = [{'_id',1},{key,[{subkey,<<"value1">>}]}],
	Expected = {key,{subkey,<<"value1">>},'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_pairs_with_binary_string_keys_has_sub_tuple_test() -> 
	TestValue = [{<<"_id">>,1},{<<"key">>,[{<<"subkey">>,<<"value1">>}]}],
	Expected = {key,{subkey,<<"value1">>},'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_with_sub_tuple_test() -> 
	TestValue = {'_id',1,key,{subkey,<<"value1">>}},
	Expected = [{key,[{subkey,<<"value1">>}]},{'_id',1}],
	Result = mongo_converter:tuple_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_sub_tuple_test() -> 
	TestValue = [{'_id',1,key,{subkey,<<"value1">>}}],
	Expected = [{key,[{subkey,<<"value1">>}]},{'_id',1}],
	Result = mongo_converter:tuples_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_tuple_pairs_with_atom_keys_has_list_of_sub_tuple_test() -> 
	TestValue =[{'_id',1},{key1,[[{subKey1,<<"subKey1Value1">>}],[{subKey2,<<"subKey2Value1">>}]]}],
	Expected = {key1,[{subKey1,<<"subKey1Value1">>},{subKey2,<<"subKey2Value1">>}],'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_pairs_with_binary_string_keys_has_list_of_sub_tuple_test() -> 
	TestValue =[{<<"_id">>,1},{<<"key1">>,[[{<<"subKey1">>,<<"subKey1Value1">>}],[{<<"subKey2">>,<<"subKey2Value1">>}]]}],
	Expected = {key1,[{subKey1,<<"subKey1Value1">>},{subKey2,<<"subKey2Value1">>}],'_id',1},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_with_list_of_sub_tuple_test() -> 
	TestValue = {'_id',1,key,{subkey,<<"value1">>}},
	Expected = [{key,[{subkey,<<"value1">>}]},{'_id',1}],
	Result = mongo_converter:tuple_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_list_of_sub_tuple_test() -> 
	TestValue = [{'_id',1,key,{subkey,<<"value1">>}}],
	Expected = [{key,[{subkey,<<"value1">>}]},{'_id',1}],
	Result = mongo_converter:tuples_to_tuple_pairs(TestValue),
	?assert(Result == Expected).

when_tuple_pair_contains_an_object_id_as_binary_string_value_test() -> 
	TestValue = [{<<"key">>,[{<<"$oid">>,<<"10283042123123">>}]}],
	Expected =  {key,{<<16,40,48,66,18,49,35>>}},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).

when_tuple_pair_contains_an_object_id_as_atom_value_test() -> 
	TestValue = [{'key',[{'$oid',<<"10283042123123">>}]}],
	Expected =  {key,{<<16,40,48,66,18,49,35>>}},
	Result = mongo_converter:tuple_pairs_to_tuple(TestValue),
	?assert(Result == Expected).
