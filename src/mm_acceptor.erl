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
-export([start/2, accept_loop/2]).

%%
%% API Functions
%%

%%start listen server
start(Port, Func) ->
  case (do_init(Port, Func)) of
    {ok, ListenSocket} ->
      accept_loop(ListenSocket, Func);
    _Els ->
      error
  end.

%%listen port
do_init(Port, Func) when is_list(Port) ->
  start(list_to_integer(Port), Func)
;
do_init([Port], Func) when is_atom(Port) ->
  start(list_to_integer(atom_to_list(Port)), Func)
;
do_init(Port, Func) when is_integer(Port) ->
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
accept_loop(ListenSocket, Func) ->
  case (gen_tcp:accept(ListenSocket, 10000)) of
    {ok, Socket} ->
      Func(Socket),
      ?MODULE:accept_loop(ListenSocket, Func);
    {error, Reason} ->
      ?MODULE:accept_loop(ListenSocket, Func);
    {exit, Reason} ->
      ?MODULE:accept_loop(ListenSocket, Func)
  end.

