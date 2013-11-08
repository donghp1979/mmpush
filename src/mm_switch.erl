-module(mm_switch).
-export([loop/0, start/0, init/0]).
-compile(export_all).


start() ->
	Pid = spawn(fun() ->
			?MODULE:init(),
			?MODULE:loop() 
		end),
	register(mm_switch, Pid).

init() ->
	io:format("Init......~n"),
	ets:new(clients, [set,public,named_table]),
	ets:new(tokens, [duplicate_bag, public, named_table]),
	ok.

loop() ->
	receive 
		{add_client, Id, Token, Pid} ->
			io:format("add client [~p]~n", [{Id, Token, Pid}]),
			ets:insert(clients, {Id, Token, Pid}),
			ets:insert(tokens, {Token, Id}),
			loop();
		{remove_client, Id} ->
			io:format("remove client [~p]~n", [Id]),
			case ets:lookup(clients, Id) of
				[{Id, Token, _Pid}] ->
					ets:delete_object(tokens, {Token, Id}),
					ets:delete(clients, Id);
				_ -> void
			end,
			loop();
		{sendmsg, To, Message} ->
			io:format("send to client [~p] [~p]~n", [To, Message]),
			case ets:lookup(tokens, To) of
				[{To, Id}|L] ->
					send_msg_to([{To, Id}|L], Message);
				[] -> void
			end,
			loop();
		Others ->
			io:format("Unknow message [~p]~n", [Others]),
			loop()
	end.

summary() ->
	io:format("clients ~p~n", [ets:match_object(clients,{'_','_', '_'})]),
	io:format("tokens ~p~n", [ets:match_object(tokens,{'_','_'})]).

send_msg_to([], _Message) -> ok;
send_msg_to([{To, Id}|L], Message) ->
	case ets:lookup(clients, Id) of
		[{Id, To, Pid}] ->
			send_msg_to_pid(Pid, {To, Id, Message});
		[] -> 
			io:format("Not found clients ~p~n", [{Id, Message}]);
		_Others ->
			io:format("ets:lookup error ~p~n", [{_Others, Id, Message}])
	end,
	send_msg_to(L, Message).

send_msg_to_pid(Pid, Message) when is_pid(Pid) ->
	Pid!{sendmsg, Message};
send_msg_to_pid(Pid, Message) ->
	io:format("send ~p to ~p ~n", [Message, Pid]).

%% For Test 

client_loop(Id, Token) ->
	receive 
		{sendmsg, Message} ->
			io:format("Client ~p get a message ~p~n", [self(), {Id, Token, Message}]),
			client_loop(Id, Token);
		_ -> client_loop(Id, Token) 
	end.	

new_client(Id, Token) ->
	Pid = spawn(fun() -> client_loop(Id, Token) end),
	?MODULE!{add_client, Id, Token, Pid}.

setup_test() ->
	mm_switch:new_client(1, "Client1"),
	mm_switch:new_client(2, "Client2"),
	mm_switch:new_client(3, "Client3"),
	mm_switch:new_client(4, "Client1"),
	ok.

