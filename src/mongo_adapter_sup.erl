-module(mongo_adapter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Connection = [{url,"127.0.0.1"}, {port,27017},{database,'local'}],
	Appender = {mongo_adapter, 
						{mongo_adapter, start_link, [Connection]}, 
							permanent, 
							5000,
							worker, 
							[mongo_adapter]},
	Children = [Appender],
    {ok, { {one_for_one, 5, 10}, Children} }.

