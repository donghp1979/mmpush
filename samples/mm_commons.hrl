%%%-------------------------------------------------------------------
%%% @author Fishman
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 十一月 2013 下午8:21
%%%-------------------------------------------------------------------
-author("Fishman").
-define(CMD_SEND, 1).
-define(CMD_SEND_RESP, 8).
-define(CMD_LOGIN, 2).
-define(CMD_LOGIN_RESP, 9).
-define(TOKEN_LENGTH,32).

-record(mm_request, {
  cmd,
  id,
  expire,
  token,
  payload
}).

-record(mm_resp, {
  cmd,
  status,
  id
}).

-record(clientinfo, {
  id,
  token,
  socket,
  pid
}).

