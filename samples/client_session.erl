%% Author: Administrator
%% Created: 2012-2-16
%% Description: TODO: Add description to client_session
-module(client_session).
-behavior(gen_server).
%%
%% Include files
%%
-include("mm_commons.hrl").
%%
%% Exported Functions
%%
-export([init/1, start_link/1, handle_info/2, handle_call/3, terminate/2]).
-export([process_msg/1]).
%%
%% API Functions
%%
start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], [])
%gen_server:start_link({local,?MODULE}, ?MODULE, [Id],[])
.


init(Id) ->
  State = #clientinfo{id = Id},
  {ok, State}.

%%handle message send from room
handle_info({tcp_closed, Socket}, State) ->
  io:format("client sockt closed ~n"),
%%  chat_room:logout(State),
  {noreply, State};

handle_info({dwmsg, Message}, State) ->
  io:format("client_session dwmsg recived ~p~n", [Message]),
  case gen_tcp:send(State#clientinfo.socket, mm_commons:encode_packet(Message)) of
    ok ->
      io:format("client_session dwmsg sended ~n");
    {error, Reason} ->
      io:format("client_session dwmsg sended error ~p ~n", Reason)
  end,
  {noreply, State};
%%handle message recived from client
%handle_info(Message,State) when is_binary(Message)->
handle_info({bind, Socket}, State) ->
  io:format("client_session bind socket ~n"),
  NewState = State#clientinfo{socket = Socket},
  io:format("NewState ~p~n", [NewState]),
  {noreply, NewState};
%to start reciving
%handle_info({start,Pid},State)->
%		io:format("client_session:reciving start...~p~n",[State#clientinfo.socket]),
%		NewState=State#clientinfo{pid=Pid},
%        process_msg(NewState),					
%	{noreply,State};
handle_info({tcp, Socket, Data}, State) ->
  io:format("client_session tcp data recived ~p~n", [Data]),
  {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
  chat_room:logout(State),
  void;
handle_info(stop, State) ->
  io:format("client stop"),
  {stop, normal, stopped, State};
handle_info(Request, State) ->
  io:format("client_session handle else ~p~n", [Request]),
  {noreply, State}
.

handle_call(Request, From, State) ->
  {reply, ok, State}.

handle_cast(Request, State) ->
  {noreply, State}.
terminate(_Reason, State) ->
  ok.

%%
%% Local Functions
%%
process_msg(State) ->
  io:format("client_session:process_msg SOCKET:~p ~n", [State#clientinfo.socket]),
  case gen_tcp:recv(State#clientinfo.socket, 0) of
    {ok, Message} ->
      io:format("recived ~p ~n", [Message]),
      %io:format("msg recived ~p~n",[Message]),
      %NewMsg = #message{type = msg, from = State#clientinfo.id, content = Message},
      %chat_room:broadCastMsg(NewMsg),
      process_msg(State);
    {error, closed} ->
      io:format("client_session:recive error ~n"),
      process_msg(State);
    Any ->
      io:format("client_session:recive any ~n"),
      process_msg(State)
  end
.

