-module(start).
-include("user.hrl").
-include("post.hrl").
-include("thread.hrl").
-export([start/0]).

start() ->
	odbc:start(),
	{ok, Ref} = odbc:connect("DSN=erlforum", []),
	user:creat_user_table(Ref),
	post:creat_post_table(Ref),
	thread:creat_thread_table(Ref),
	cookie:creat_cookie_table(Ref)
	.
