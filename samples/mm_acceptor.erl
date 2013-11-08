%% Author: Administrator
%% Created: 2012-2-18
%% Description: TODO: Add description to chat_acceptor
-module(mm_acceptor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1, accept_loop/1]).

%%
%% API Functions
%%

%%start listen server
start(Port) ->
  case (do_init(Port)) of
    {ok, ListenSocket} ->
      accept_loop(ListenSocket);
    _Els ->
      error
  end.

%%listen port
do_init(Port) when is_list(Port) ->
  start(list_to_integer(Port))
;
do_init([Port]) when is_atom(Port) ->
  start(list_to_integer(atom_to_list(Port)))
;
do_init(Port) when is_integer(Port) ->
  Options = [binary,
    {packet, 0},
    {reuseaddr, true},
    {backlog, 1024},
    {active, true}],
  case gen_tcp:listen(Port, Options) of
    {ok, ListenSocket} ->
      {ok, ListenSocket};
    {error, Reason} ->
      {error, Reason}
  end.

%%accept client connection
accept_loop(ListenSocket) ->
  case (gen_tcp:accept(ListenSocket, 3000)) of
    {ok, Socket} ->
      process_clientSocket(Socket),
      ?MODULE:accept_loop(ListenSocket);
    {error, Reason} ->
      ?MODULE:accept_loop(ListenSocket);
    {exit, Reason} ->
      ?MODULE:accept_loop(ListenSocket)
  end.

%%process client socket
%%we should start new thread to handle client
%%generate new id using id_generator
process_clientSocket(Socket) ->
  io:format("Socket = ~p~n", [Socket]),
  Record = mm_switch:getPid(),
  mm_switch:bindPid(Record, Socket),
  ok.
%%
%% Local Functions
%%

