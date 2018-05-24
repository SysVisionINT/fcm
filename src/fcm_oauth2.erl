%%
%% Copyright 2018 SysVision, LDA - http://www.sysvision.pt/
%%
-module(fcm_oauth2).

-behaviour(gen_server).

%% ====================================================================
%% Callback functions
%% ====================================================================
-callback handle_task(Task::any()) -> ok.

%% ====================================================================
%% Constants
%% ====================================================================
-define(SERVER, {local, ?MODULE}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([start_link/0]).

start_link() -> gen_server:start_link(?SERVER, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

init([]) ->
	error_logger:info_msg("~p [~p] starting...~n", [?MODULE, self()]),
	{ok, #state{}}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
