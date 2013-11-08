%% Author: Administrator
%% Created: 2012-2-16
%% Description: TODO: Add description to id_generator
-module(id_generator).
-behavior(gen_server).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/0, getnewid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(ids, {idtype, ids}).
-record(state, {}).
%%
%% API Functions
%%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])
.

init([]) ->
  mnesia:start(),
  io:format("Started"),
  mnesia:create_schema([node()]),
  case mnesia:create_table(ids, [{type, ordered_set},
    {attributes, record_info(fields, ids)},
    {disc_copies, []}
  ]) of
    {atomic, ok} ->
      {atomic, ok};
    {error, Reason} ->
      io:format("create table error")
  end,
  {ok, #state{}}
.

getnewid(IdType) ->
  %case mnesia:wait_for_tables([tbl_clientid], 5000) of
  %	ok->
  %		gen_server:call(?MODULE, {getid,IdType});
  %	{timeout,_BadList}->
  %		{timeout,_BadList};
  %	{error,Reason}->
  %		{error,Reason}
  %end
  mnesia:force_load_table(ids),
  gen_server:call(?MODULE, {getid, IdType})
.

%%generate new Id with given type
handle_call({getid, IdType}, From, State) ->
  F = fun() ->
    Result = mnesia:read(ids, IdType, write),
    case Result of
      [S] ->
        Id = S#ids.ids,
        NewClumn = S#ids{ids = Id + 1},
        mnesia:write(ids, NewClumn, write),
        Id;
      [] ->
        NewClumn = #ids{idtype = IdType, ids = 2},
        mnesia:write(ids, NewClumn, write),
        1
    end
  end,
  case mnesia:transaction(F) of
    {atomic, Id} ->
      {atomic, Id};
    {aborted, Reason} ->
      io:format("run transaction error ~1000.p ~n", [Reason]),
      Id = 0;
    _Els ->
      Id = 1000
  end,
  {reply, Id, State}
.


handle_cast(_From, State) ->
  {noreply, ok}.

handle_info(Request, State) ->
  {noreply, ok}.

terminate(_From, State) ->
  ok.

code_change(_OldVer, State, Ext) ->
  {ok, State}.
%%
%% Local Functions
%%

