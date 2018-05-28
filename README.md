# fcm
Firebase Cloud Messaging

fcm
=======
*Firebase Cloud Messaging server HTTP v1 protocol Erlang implementation*

Installation
------------

Using rebar3:

```
{deps, [
	{fcm, {git, "https://github.com/SysVisionINT/fcm.git", {branch, "master"}}}
]}.
```

Setup fcm
-------------

After starting fcm, the setup function must be called, before messages can be sent.

```
#!erlang
fcm:setup(ProjectId :: binary(), ClientEmail :: binary(), RsaPrivateKey :: binary()) -> ok | invalid_key.
```

The RsaPrivateKey must be in the "RSA PRIVATE KEY" format.
To convert the key provided by Google use:

```
openssl rsa -in server.key -out server_new.key
```

Sending a message
--------------

In order to send a message, first include the fcm.hrl file:

```
#!erlang
-include_lib("fcm/include/fcm.hrl").
```

Create a ```#fcm_message{}``` according to the [Firebase Cloud Messaging](https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages) message specification, and use the send function.

```
#!erlang
fcm:send(Message :: #fcm_message{}) -> ok | error.
```
