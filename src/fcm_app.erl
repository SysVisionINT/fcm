%%
%% Copyright 2018 SysVision, LDA - http://www.sysvision.pt/
%%
-module(fcm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    {ok, Pid} = fcm_sup:start_link(),
	{ok, Pid}.

stop(_State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================
