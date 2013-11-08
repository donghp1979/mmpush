%%%-------------------------------------------------------------------
%%% @author Fishman
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 十一月 2013 下午8:28
%%%-------------------------------------------------------------------
-module(mm_commons).
-author("Fishman").
-include("mm_commons.hrl").

%% API
-export([bin_to_hexstr/1, hexstr_to_bin/1]).
-export([encode_packet/1, decode_packet/1, format_token/1]).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).

hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
  hexstr_to_bin(T, [V | Acc]).


%% Packet = [<<1:8, MsgId/binary, Expiry:4/big-unsigned-integer-unit:8, 32:16/big,BinToken/binary,PayloadLength:16/big,BinPayload/binary>>]

encode_packet(Request) when is_record(Request, mm_request) ->
  PayloadLength = size(Request#mm_request.payload),
  Cmd = Request#mm_request.cmd,
  MsgId = Request#mm_request.id,
  Expiry = Request#mm_request.expire,
  BinToken = Request#mm_request.token,
  BinPayLoad = Request#mm_request.payload,
  {ok, <<Cmd:8, MsgId:4/binary, Expiry:4/big-unsigned-integer-unit:8, 32:16/big, BinToken:32/binary, PayloadLength:16/big, BinPayLoad/binary>>};

encode_packet(Response) when is_record(Response, mm_resp) ->
  Cmd = Response#mm_resp.cmd,
  Status = Response#mm_resp.status,
  MsgId = Response#mm_resp.id,
  {ok, <<Cmd:8, Status:8, MsgId:32>>}.


decode_packet(<<Cmd:8, Bin/binary>>) when Cmd == ?CMD_LOGIN;Cmd == ?CMD_SEND ->
  case Bin of
    <<MsgId:4/big-unsigned-integer-unit:8, Expiry:4/big-unsigned-integer-unit:8, TokenLength:16/big, BinToken:32/binary, PayLoadLength:16/big, BinRest/binary>> ->
      case BinRest of
        <<BinPayLoad:PayLoadLength/binary, _RestBin/binary>> ->
          Request = #mm_request{cmd = Cmd, id = MsgId, expire = Expiry, token = BinToken, payload = BinPayLoad},
          case TokenLength of
            ?TOKEN_LENGTH ->
              {ok, Request, _RestBin};
            _ -> {error, token_length_error}
          end;
        _Others ->
          {more, need_more_payload}
      end;
    _ -> {more, need_more_body}
  end;

decode_packet(<<Cmd:8, Bin/binary>>) when Cmd == ?CMD_LOGIN_RESP;Cmd == ?CMD_SEND_RESP ->
  case Bin of
    <<Status:8, MsgId:4/binary, _RestBin/binary>> ->
      Resp = #mm_resp{cmd = Cmd, status = Status, id = MsgId},
      {ok, Resp, _RestBin};
    _ -> {more, need_more_body}
  end.

format_token(BinToken) when is_binary(BinToken), size(BinToken) == ?TOKEN_LENGTH ->
  BinToken;
format_token(BinToken) when is_binary(BinToken), size(BinToken) < ?TOKEN_LENGTH ->
  list_to_binary(binary_to_list(BinToken) ++ [0 || _ <- lists:seq(1, ?TOKEN_LENGTH - size(BinToken))]).
