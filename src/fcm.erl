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
-module(fcm).

-include("fcm.hrl").

-define(FCM_MESSAGE_URL(Project), "https://fcm.googleapis.com/v1/projects/" ++ binary_to_list(Project) ++ "/messages:send").

%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/3, send/1]).

-spec setup(ProjectId :: binary(), ClientEmail :: binary(), RsaPrivateKey :: binary()) -> ok | invalid_key.
setup(ProjectId, ClientEmail, RsaPrivateKey) ->
	case check_rsa_key(RsaPrivateKey) of
		true -> fcm_oauth2:setup(ProjectId, ClientEmail, RsaPrivateKey);
		false -> invalid_key
	end.

-spec send(Message :: #fcm_message{}) -> ok | error.
send(Message) ->
	Json = to_json(Message),
	Body = jsondoc:encode(Json),
	case fcm_oauth2:get_access_token() of
		{ok, Token} ->
			Headers = [{"Authorization", "Bearer " ++ binary_to_list(Token)}],
			{ok, ProjectId} = fcm_oauth2:get_project_id(),
			Request = {?FCM_MESSAGE_URL(ProjectId), Headers, "application/json", Body},
			case httpc:request(post, Request, [], []) of
				{ok, {{_, 200, _}, _, _}} ->
					ok;
				Other ->
					error_logger:error_msg("~p:send(~p): error sending message: ~p~n", [?MODULE, Message, Other]),
					error
			end;
		_ -> error
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_rsa_key(<< "-----BEGIN RSA PRIVATE KEY-----", _/binary >>) -> true;
check_rsa_key(_) -> false.

to_json(null) -> null;
to_json(undefined) -> null;
to_json(Value) when is_bitstring(Value) -> Value;
to_json(Value) when is_integer(Value) -> integer_to_binary(Value);
to_json(Value) when is_float(Value) -> float_to_binary(Value);
to_json(Value) when is_boolean(Value) -> atom_to_binary(Value, utf8);
to_json(List) when is_list(List) -> lists:map(fun to_json/1, List);
to_json(Message) when is_record(Message, fcm_message) ->
	[{message, record_to_json(Message)}];
to_json(#fcm_map_entry{key=Key, value=Value}) ->
	AtomKey =
		if
			is_bitstring(Key) -> binary_to_atom(Key, utf8);
			is_atom(Key) -> Key
		end,
	{AtomKey, to_json(Value)};
to_json(Tuple) when is_tuple(Tuple), tuple_size(Tuple) > 1 ->
	record_to_json(Tuple).

record_to_json(Record) ->
	[Name|Values] = erlang:tuple_to_list(Record),
	Fields = get_record_fields(Name),
	[{Field, to_json(Value)} || {Field, Value} <- lists:zip(Fields, Values), Value =/= undefined].

get_record_fields(fcm_message) -> record_info(fields, fcm_message);
get_record_fields(fcm_notification) -> record_info(fields, fcm_notification);
get_record_fields(fcm_android_config) -> record_info(fields, fcm_android_config);
get_record_fields(fcm_android_notification) -> record_info(fields, fcm_android_notification);
get_record_fields(fcm_webpush_config) -> record_info(fields, fcm_webpush_config);
get_record_fields(fcm_apns_config) -> record_info(fields, fcm_apns_config).
