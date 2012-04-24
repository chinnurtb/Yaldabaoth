-module(player_handler).
-author('Piotr Nosek <xfenek@gmail.com>').
-behaviour(gen_server).

-include_lib("yaldabaoth/include/player.hrl").
-include_lib("yaldabaoth/include/control_data.hrl").

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% ===================================================================
%% Callbacks
%% ===================================================================

init(State) ->
    {ok, SymmetricKey} = do_handshake(State#player_state.control_socket),
    {ok, State#player_state{ symmetric_key = SymmetricKey }}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

%% ===================================================================
%% Private functions
%% ===================================================================

-spec do_handshake(term()) -> {ok, binary()}.
do_handshake(Socket) ->
    io:format("Performing handshake...~n", []),
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    Msg = data_codec:decode(Packet),
    G = crypto:mpint(Msg#hello_msg.g),
    P = crypto:mpint(Msg#hello_msg.p),
    A = crypto:mpint(Msg#hello_msg.a),
    io:format("Got params: G:~p, P:~p, A:~p~n", [crypto:erlint(G), crypto:erlint(P), crypto:erlint(A)]),
    {MyPublicKey, MyPrivateKey} = crypto:dh_generate_key([P, G]),
    SymmetricKey = crypto:dh_compute_key(A, MyPrivateKey, [P, G]),
    io:format("Sending...~n", []),
    gen_tcp:send(Socket, data_codec:encode(#hello_rsp{ b = crypto:erlint(MyPublicKey) })),
    io:format("Done!~n", []),
    {ok, SymmetricKey}.
