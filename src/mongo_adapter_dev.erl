-module(mongo_adapter_dev).
-export([start/0, stop/0]).

start() ->
    Connection = [{url,"127.0.0.1"}, {port,27017},{database,'local'}],
	mongo_adapter:start_link(Connection).

stop() ->
    ok.