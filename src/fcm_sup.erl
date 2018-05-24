%%
%% Copyright 2018 SysVision, LDA - http://www.sysvision.pt/
%%
-module(fcm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
	FcmOauth2 = #{id => fcm_oauth2,
	              start => {fcm_oauth2, start_link, []},
	              restart => permanent,
	              shutdown => 2000,
	              type => worker,
	              modules => [fcm_oauth2]},
    {ok, {#{strategy => one_for_one, intensity => 1, period => 5}, [FcmOauth2]}}.
