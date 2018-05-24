%%
%% Copyright 2018 SysVision, LDA - http://www.sysvision.pt/
%%
-module(fcm).

%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/2]).

setup(ClientEmail, RsaPrivateKey) ->
	case check_rsa_key(RsaPrivateKey) of
		true -> fcm_oauth2:setup(ClientEmail, RsaPrivateKey);
		false -> invalid_key
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_rsa_key(<< "-----BEGIN RSA PRIVATE KEY-----", _/binary >>) -> true;
check_rsa_key(_) -> false.
