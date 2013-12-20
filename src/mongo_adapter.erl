-module(mongo_adapter).
-behaviour (gen_server).

-export([upsert/1,delete/1,find/1,command/1]).

-export([start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,stop/0]).

-record(context, {
	connection :: pid(),
	database   :: atom()
}).

start_link(ConnectionTuple) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, ConnectionTuple, []).

init([{url,Url},{port,Port},{database,Database}]) ->
	{ok, Connection} = mongo_connection:start_link({Url, Port}, []),
	{ok, #context{connection=Connection,database=Database}}.

upsert({Collection,Value}) ->
	ValueAsTuple = mongo_converter:tuple_pairs_to_tuple(Value),
	gen_server:cast(?MODULE,{upsert, {Collection,ValueAsTuple}}).

delete({Collection,Query}) ->
	gen_server:call(?MODULE,{delete, {Collection,Query}}).

find({Collection,Query}) ->
	QueryAsTuple = mongo_converter:tuple_pairs_to_tuple(Query),
	gen_server:call(?MODULE,{find, {Collection,QueryAsTuple}}).

command(Command) ->
	gen_server:call(?MODULE,{command,Command}).

stop() ->
	gen_server:cast(?MODULE,stop).
	
% Callbacks

%handle_call(Atom, From, State) ->
handle_call({find, {Collection,Query}}, _, Context) ->
	Result = mongo_call(Context, fun() ->
		find(Collection,Query)
	end),
	ResultAsTuple = mongo_converter:tuples_to_tuple_pairs(Result),
 	{reply, ResultAsTuple, Context};

handle_call({command,Command},_,Context) ->
	Result = mongo_call(Context,fun() ->
		mongo:command(Command)
	end),
	{reply,Result,Context}.

%handle_cast(Msg, State) ->
handle_cast({upsert,{Collection,Value}}, Context) ->
	mongo_call(Context, fun() ->
		case mongo_types:has_id(Value) of
			{true,Id} ->
				mongo:update(Collection, {'_id',Id}, Value, true);
			_ -> 
				mongo:insert(Collection, Value)
		end
	end),
    {noreply, Context};

handle_cast(stop,State) ->
	{stop,normal,State};

handle_cast(_, State) ->
    {noreply, State}.

%handle_info(Info, State) ->
handle_info(_, State) ->
    {noreply, State}.

%terminate(Reason, State) ->
terminate(_, _) ->
    ok.

%code_change(OldVsn, State, Extra) ->
code_change(_, State, _) ->
    {ok, State}. 

mongo_call(Context,Call) ->
	mongo:do(safe, master, Context#context.connection, Context#context.database, Call).

find(Collection, Selector) ->
	find(Collection, Selector, []).

find(Collection, Selector, Projector) ->
	Cursor = mongo:find(Collection, Selector, Projector),
	Result = mongo_cursor:rest(Cursor),
	mongo_cursor:close(Cursor),
	Result.