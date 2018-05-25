%%
%% fcm - Firebase Cloud Messaging
%% 
%% Copyright (C) 2018 SysVision - Consultadoria e Desenvolvimento em Sistemas de Informática, Lda.  
%% 
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
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
-define(TOKEN_EXPIRATION_TIME, 3600). % In seconds
-define(OAUTH2_URL, <<"https://www.googleapis.com/oauth2/v4/token">>).
-define(FCM_SCOPE, <<"https://www.googleapis.com/auth/firebase.messaging">>).
-define(GRANT_TYPE, "urn:ietf:params:oauth:grant-type:jwt-bearer").

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([setup/3, get_access_token/0, get_project_id/0]).

%% ====================================================================
%% API functions
%% ====================================================================
start_link() -> gen_server:start_link(?SERVER, ?MODULE, [], []).

setup(ProjectId, Iss, RsaPrivateKey) ->
	gen_server:call(?MODULE, {setup, ProjectId, Iss, RsaPrivateKey}).

get_access_token() ->
	gen_server:call(?MODULE, {get_access_token}).

get_project_id() ->
	gen_server:call(?MODULE, {get_project_id}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {project_id, iss, rsa_private_key, token, expiration}).

init([]) ->
	error_logger:info_msg("~p [~p] starting...~n", [?MODULE, self()]),
	{ok, #state{}}.

handle_call({setup, ProjectId, Iss, RsaPrivateKey}, _From, _State) ->
	{reply, ok, #state{project_id=ProjectId, iss=Iss, rsa_private_key=RsaPrivateKey, expiration=0}};

handle_call({get_access_token}, _From, State=#state{iss=Iss, rsa_private_key=RsaPrivateKey})
  when not is_binary(Iss); not is_binary(RsaPrivateKey) ->
	{reply, {error, not_setup}, State};

handle_call({get_access_token}, _From, State=#state{iss=Iss, rsa_private_key=RsaPrivateKey, token=Token, expiration=Expiration}) ->
	Now = erlang:system_time(seconds),
	case Now > Expiration of
		true ->
			Exp = integer_to_binary(Now + ?TOKEN_EXPIRATION_TIME),
			Iat = integer_to_binary(Now),
			Claims = [{iss, Iss}, {scope, ?FCM_SCOPE}, {aud, ?OAUTH2_URL}, {exp, Exp}, {iat, Iat}],
			Jwt = jwerl:sign(Claims, rs256, RsaPrivateKey),
			Body = "grant_type=" ++ ?GRANT_TYPE ++ "&assertion=" ++ binary_to_list(Jwt),
			Request = {binary_to_list(?OAUTH2_URL), [], "application/x-www-form-urlencoded", Body},
			case httpc:request(post, Request, [], []) of
				{ok, {{_, 200, _}, _, RespBody}} ->
					{Json} = jsondoc:decode(RespBody),
					NewToken = proplists:get_value(<<"access_token">>, Json),
					{reply, {ok, NewToken}, State#state{token=NewToken, expiration=Now + ?TOKEN_EXPIRATION_TIME}};
				Other ->
					error_logger:error_msg("~p:handle_call({get_access_token}, ...): error getting access token: ~p~n", [?MODULE, Other]),
					{reply, {error, access_token}, State}
			end;
		false ->
			{reply, {ok, Token}, State}
	end;

handle_call({get_project_id}, _From, State=#state{project_id=undefined}) ->
	{reply, {error, not_setup}, State};
handle_call({get_project_id}, _From, State=#state{project_id=ProjectId}) ->
	{reply, {ok, ProjectId}, State};

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
