-module(front_server).
-author('Piotr Nosek <xfenek@gmail.com>').
-behavior(gen_server).

-include_lib("yaldabaoth/include/player.hrl").

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/1]).

-define(TCP_OPTIONS, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
		port,
		ip=any,
		lsocket=null,
		accept_pid}).

%% ===================================================================
%% API functions
%% ===================================================================

start(Port) ->
	State = #server_state{port = Port},
	gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

accept_loop({Server, LSocket}) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(Server, {accepted, self()}),
	gen_server:start(player_handler, #player_state{ control_socket = Socket }, []).

%% ===================================================================
%% Callbacks
%% ===================================================================

init(State = #server_state{port=Port}) ->
    crypto:start(),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    io:format("Accepted.~n", []),
	{noreply, accept(State)}.

%% ===================================================================
%% Other callbacks
%% ===================================================================

handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

%% ===================================================================
%% Private functions
%% ===================================================================

accept(State = #server_state{lsocket=LSocket}) ->
	Pid = proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket}]),
	State#server_state{accept_pid = Pid}.
