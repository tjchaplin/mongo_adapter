-module(mongo_adapter).
-behaviour (gen_server).

-export([upsert/1,upsert_sync/1,insert/1,insert_sync/1,delete/1,find/1,find_one/1,command/1]).

-export([start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,stop/0]).

-record(context, {
	connection :: pid(),
	database   :: atom()
}).

start_link(ConnectionOptions) ->
	dependency_manager:ensure_started(bson),
	dependency_manager:ensure_started(mongodb),
	gen_server:start_link({local, ?MODULE}, ?MODULE, ConnectionOptions, []).

init([{url,Url},{port,Port},{database,Database}]) ->
	{ok, Connection} = mongo_connection:start_link({Url, Port}, []),
	{ok, #context{connection=Connection,database=Database}}.

upsert_sync({Collection,Value}) ->
	case mongo_types:has_id(Value) of
		unknown ->
			insert_sync({Collection, Value});
		Id -> 
			upsert_sync({Collection,Value,[{'_id',Id}]})
	end;

upsert_sync({Collection,Value,Query}) ->
	ValueAsTuple = tuple_pairs_converter:convert(Value),
	QueryAsTuple = tuple_pairs_converter:convert(Query),
	gen_server:call(?MODULE,{upsert, {Collection,ValueAsTuple,QueryAsTuple}}).

insert_sync({Collection,Value}) ->
	ValueAsTuple = tuple_pairs_converter:convert(Value),
	gen_server:call(?MODULE,{insert, {Collection,ValueAsTuple}}).

upsert({Collection,Value}) ->
	case mongo_types:has_id(Value) of
		unknown ->
			insert({Collection, Value});
		Id -> 
			upsert({Collection, Value,[{'_id',Id}]})
	end;

upsert({Collection,Value,Query}) ->
	ValueAsTuple = tuple_pairs_converter:convert(Value),
	QueryAsTuple = tuple_pairs_converter:convert(Query),
	gen_server:cast(?MODULE,{upsert, {Collection,ValueAsTuple,QueryAsTuple}}).

insert({Collection,Value}) ->
	ValueAsTuple = tuple_pairs_converter:convert(Value),
	gen_server:cast(?MODULE,{insert, {Collection,ValueAsTuple}}).

delete({Collection,Query}) ->
	QueryAsTuple = tuple_pairs_converter:convert(Query),
	gen_server:cast(?MODULE,{delete, {Collection,QueryAsTuple}}).

find({Collection,Query}) ->
	QueryAsTuple = tuple_pairs_converter:convert(Query),
	gen_server:call(?MODULE,{find, {Collection,QueryAsTuple}}).

find_one(TupleQuery) ->
	Result = find(TupleQuery),
	case Result of
		[Item|_Tail] -> Item;
		_ -> []
	end.

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
	ResultAsTuple = tuple_converter:convert(Result),
 	{reply, ResultAsTuple, Context};

handle_call({command,Command},_,Context) ->
	Result = mongo_call(Context,fun() ->
		mongo:command(Command)
	end),
	{reply,Result,Context};

handle_call({upsert,{Collection,Value,Query}},_From, Context) ->
	Result = mongo_call(Context, fun() ->
		mongo:update(Collection, Query, Value, true)
	end),
    {reply, Result, Context};

handle_call({insert,{Collection,Value}}, _From,Context) ->
	Result = mongo_call(Context, fun() ->
				mongo:insert(Collection, Value)
			end),
	{reply, Result, Context}.

%handle_cast(Msg, State) ->
handle_cast({upsert,{Collection,Value,Query}}, Context) ->
	mongo_call(Context, fun() ->
		mongo:update(Collection, Query, Value, true)
	end),
    {noreply, Context};

handle_cast({insert,{Collection,Value}}, Context) ->
	mongo_call(Context, fun() ->
		mongo:insert(Collection, Value)
	end),
    {noreply, Context};

handle_cast({delete,{Collection,Query}}, Context) ->
	mongo_call(Context, fun() ->
		mongo:delete(Collection, Query)
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
	try
		mongo:do(safe, master, Context#context.connection, Context#context.database, Call)
	catch 
		exit:{write_failure,11000,ErrorMessage} -> 
	    	{error,{duplicate_key_error,ErrorMessage}};
		exit:{write_failure,_ErrorCode,ErrorMessage} ->
	    	{error,{write_failure,ErrorMessage}};
		exit:Error -> 
	    	{error,{unknown_error,Error}}
	end.
	
find(Collection, Selector) ->
	find(Collection, Selector, []).

find(Collection, Selector, Projector) ->
	Cursor = mongo:find(Collection, Selector, Projector),
	Result = mongo_cursor:rest(Cursor),
	mongo_cursor:close(Cursor),
	Result.