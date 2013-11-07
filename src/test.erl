-module(test).
-compile(export_all).

print() ->
	print(0).
	
print(N) when is_integer(N)->
	io:format("Hello,~p~n", [N]),
	print(N+1);
print(Msg) ->
	io:format("Hello,~p~n", [Msg]).