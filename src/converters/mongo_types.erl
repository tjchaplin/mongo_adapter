-module(mongo_types).
-export([binary_string_to_objectid/1,objectid_to_binary_string/1,has_id/1,object_id/0]).

has_id(TuplePair) ->
    proplists:get_value('_id',TuplePair).

object_id() ->
	Id=mongo_id_server:object_id(),
	IdAsBinaryString = mongo_types:objectid_to_binary_string(Id),
	[{'$oid',IdAsBinaryString}].

%%This method is used to generate ObjectId from binary string.
binary_string_to_objectid(BinaryString) ->
    binary_string_to_objectid(BinaryString, []).

binary_string_to_objectid(<<>>, Result) ->
    {list_to_binary(lists:reverse(Result))};
binary_string_to_objectid(<<BS:2/binary, Bin/binary>>, Result) ->
    binary_string_to_objectid(Bin, [erlang:binary_to_integer(BS, 16)|Result]).

objectid_to_binary_string({Id}) ->
    objectid_to_binary_string(Id, []).

objectid_to_binary_string(<<>>, Result) ->
    list_to_binary(lists:reverse(Result));
objectid_to_binary_string(<<Hex:8, Bin/binary>>, Result) ->
    HexAsString = erlang:integer_to_list(Hex, 16),
    UpdatedHexAsString = case erlang:length(HexAsString) of
        1 ->
            ["0"|HexAsString];
        _ ->
            HexAsString
    end,
    objectid_to_binary_string(Bin, [UpdatedHexAsString|Result]).