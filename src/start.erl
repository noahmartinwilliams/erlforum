-module(start).
-include("include/user.hrl").
-include("include/post.hrl").
-include("include/thread.hrl").
-export([start/0]).

start() ->
	odbc:start(),
	{ok, Ref} = odbc:connect("DSN=erlforum", []),
	user:creat_user_table(Ref),
	post:creat_post_table(Ref),
	thread:creat_thread_table(Ref),
	cookie:creat_cookie_table(Ref)
	.
