-module(tuple_converter_tests).

-include_lib("eunit/include/eunit.hrl").

when_list_tuple_with_sub_tuple_with_multiple_value_test() -> 
	TestValue = [{'_id',1,key,{subkey,<<"value1">>,subkey2,<<"value2">>}}],
	Expected = [[{key,[{subkey2,<<"value2">>},{subkey,<<"value1">>}]},{'_id',1}]],
	Result = tuple_converter:convert(TestValue),
	?assert(Result == Expected).
	
when_tuple_with_single_value_test() -> 
	TestValue = {key,<<"value1">>},
	Expected = [{key,<<"value1">>}],
	Result = tuple_converter:convert_single(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_single_value_test() -> 
	TestValue = [{key,<<"value1">>}],
	Expected = [[{key,<<"value1">>}]],
	Result = tuple_converter:convert(TestValue),
	?assert(Result == Expected).

when_tuple_with_multiple_values_test() -> 
	TestValue ={'_id',1,key,<<"value1">>},
	Expected = [{key,<<"value1">>},{'_id',1}],
	Result = tuple_converter:convert_single(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_multiple_values_test() -> 
	TestValue =[{'_id',1,key,<<"value1">>}],
	Expected = [[{key,<<"value1">>},{'_id',1}]],
	Result = tuple_converter:convert(TestValue),
	?assert(Result == Expected).

when_tuple_is_array_of_strings_test() -> 
	TestValue = {'_id',1,key,[<<"value1">>,<<"value2">>]},
	Expected = [{key,[<<"value1">>,<<"value2">>]},{'_id',1}],
	Result = tuple_converter:convert_single(TestValue),
	?assert(Result == Expected).

when_list_tuple_is_array_of_strings_test() -> 
	TestValue = [{'_id',1,key,[<<"value1">>,<<"value2">>]}],
	Expected = [[{key,[<<"value1">>,<<"value2">>]},{'_id',1}]],
	Result = tuple_converter:convert(TestValue),
	?assert(Result == Expected).

when_tuple_is_array_of_ints_test() -> 
	TestValue = {'_id',1,key,[1,2]},
	Expected = [{key,[1,2]},{'_id',1}],
	Result = tuple_converter:convert_single(TestValue),
	?assert(Result == Expected).

when_list_tuple_is_array_of_ints_test() -> 
	TestValue = [{'_id',1,key,[1,2]}],
	Expected = [[{key,[1,2]},{'_id',1}]],
	Result = tuple_converter:convert(TestValue),
	?assert(Result == Expected).

when_tuple_with_sub_tuple_test() -> 
	TestValue = {'_id',1,key,{subkey,<<"value1">>}},
	Expected = [{key,[{subkey,<<"value1">>}]},{'_id',1}],
	Result = tuple_converter:convert_single(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_sub_tuple_test() -> 
	TestValue = [{'_id',1,key,{subkey,<<"value1">>}}],
	Expected = [[{key,[{subkey,<<"value1">>}]},{'_id',1}]],
	Result = tuple_converter:convert(TestValue),
	?assert(Result == Expected).

when_tuple_with_list_of_sub_tuple_test() -> 
	TestValue = {'_id',1,key,{subkey,<<"value1">>}},
	Expected = [{key,[{subkey,<<"value1">>}]},{'_id',1}],
	Result = tuple_converter:convert_single(TestValue),
	?assert(Result == Expected).

when_list_tuple_with_list_of_sub_tuple_test() -> 
	TestValue = [{'_id',1,key,{subkey,<<"value1">>}}],
	Expected = [[{key,[{subkey,<<"value1">>}]},{'_id',1}]],
	Result = tuple_converter:convert(TestValue),
	?assert(Result == Expected).