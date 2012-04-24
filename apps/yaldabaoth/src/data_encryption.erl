-module(data_encryption).

-export([encrypt/2, decrypt/2, dh_get_key/1]).

encrypt(Data, _Key) ->
    Data.

decrypt(Data, _Key) ->
    Data.

dh_get_key(_) ->
    0.
