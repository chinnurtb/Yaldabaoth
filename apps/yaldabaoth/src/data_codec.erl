-module(data_codec).

-include_lib("yaldabaoth/include/control_data.hrl").
-include_lib("yaldabaoth/include/packet_codes.hrl").

-export([encode/1, encode/2, decode/1, decode/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%%% Decode part %%%

decode(Data, Key) ->
    decode(data_encryption:decrypt(Data, Key)).

-spec decode(binary()) -> tuple().
decode(<<MsgCode:8/bytes, P:256/bytes, G:256/bytes, A:256/bytes>>) when MsgCode == ?HELLO_MSG ->
    #hello_msg{ g = binary:decode_unsigned(G),
                 p = binary:decode_unsigned(P),
                 a = binary:decode_unsigned(A) };
decode(_) ->
    {error, unknown_packet}.

%%% Encode part %%%

encode(Data, Key) ->
    data_encryption:encrypt(encode(Data), Key).

encode(#hello_rsp{b=B}) ->
    BBin = binary:encode_unsigned(B),
    KeyLen = byte_size(BBin),
    Padding = list_to_binary([ 0 || _X <- lists:seq(1,256-KeyLen) ]),
    <<?HELLO_RSP/binary, Padding/binary, BBin/binary>>.
