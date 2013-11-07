-module(test).
-compile(export_all).

print() ->
	io:format("Hello~n").

print(Msg) ->
	io:format("Hello,~p~n", [Msg]).