-module(mongo_adapter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	dependency_manager:ensure_started(bson),
	dependency_manager:ensure_started(mongodb).

stop(_State) ->
    ok.
