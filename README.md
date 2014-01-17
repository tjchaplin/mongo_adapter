mongo_adapter
=============

> An Erlang Mongo Repository

# Get Going

```erlang

% Set the Connection
ConnectionOptions = [{url,"127.0.0.1"}, {port,27017},{database,'local'}].

% Listen for requests
mongo_adapter:start_link(ConnectionOptions).

% Upsert Value
UpsertValue =  [{key,<<"value1">>},{'_id',"Id"}].
mongo_adapter:upsert({collection, UpsertValue}).

% Find Value
mongo_adapter:find({collection,[{'_id',"Id"}]}).

% Delete Value
mongo_adapter:delete({collection,[{'_id',"Id"}]}).
```

# Purpose

The existing [mongo driver](https://github.com/mongodb/mongodb-erlang) takes and returns results that are not Json compatible.  The project accepts and returns results that are Json parsable.  A popular erlang Json project is [jsx](https://github.com/talentdeficit/jsx)


# Building

##Windows

1. Get the [mongo_adapter](https://github.com/tjchaplin/mongo_adapter) source from GitHub
2. Open the command prompt
3. Cd into the directory
4. Ensure [rebar](https://github.com/rebar/rebar) is in your Path so that you can execute commands
5. Execute rebar command

```
$ rebar get-deps compile
```

##Unix

1. Get the [mongo_adapter](https://github.com/tjchaplin/mongo_adapter) source from GitHub
2. Cd into the directory
3. Make it

```
$ make
```
The library uses [mochiweb](https://github.com/mochi/mochiweb) as its lightweight webserver.


# Dependencies

The library uses [mongo_adapter](https://github.com/tjchaplin/mongo_adapter).