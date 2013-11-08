-module(test_client).
-compile(export_all).

client_loop(Sock, ClientId, N) ->
    receive 
    	{tcp, Sock, Data} ->
    		%io:format("Data: ~w ~p ~p~n", [ClientId, Sock, binary_to_list(Data)]),
    		ets:insert(msgs, {ClientId, Data}),
    		client_loop(Sock, ClientId, N+1);
    	{tcp_closed, Sock} ->
    		io:format("Client closed ~w ~p ~n", [ClientId, Sock]),
	    	gen_tcp:close(Sock);
	    _Others ->
	    	io:format("R_Others:~p~n", [_Others])
    end.


start(N) ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    ets:new(msgs, [duplicate_bag, public, named_table]),
	lists:foreach(
		fun(X) -> 
		    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 3377, 
		                                 [binary, {packet, 0}]),
		    ClientId = lists:flatten(io_lib:format("~s~w", ["client", X])),
		    ok = gen_tcp:send(Sock, ClientId),
			Pid = spawn(fun() -> client_loop(Sock, ClientId, 0) end),
			case gen_tcp:controlling_process(Sock, Pid) of
				{error, Reason} ->
					io:format("binding socket...error:~p~n", [Reason]);
				ok ->
					io:format("clientBinded~n")
			end
		end,
		lists:seq(1, N)),
	ok.



send_message(N, Message) ->
	lists:foreach(
		fun(X) ->
    		ClientId = lists:flatten(io_lib:format("~s~w", ["client", X])),
			mm_switch!{sendmsg, ClientId, Message}
		end,
		lists:seq(1, N)
		),
	ok.

send_n_message(Cnt, N, Message) ->
	lists:foreach(
		fun(X) ->
			NewMessage = lists:flatten(io_lib:format("~s~w~n", [Message, X])),
			send_message(N, NewMessage)
		end,
		lists:seq(1, Cnt)),
	ok.
