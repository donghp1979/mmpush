-module(mmServer).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%
start() ->
  mm_switch:start_link(),
  spawn(fun() -> mm_acceptor:start(3377) end),
  ok.


