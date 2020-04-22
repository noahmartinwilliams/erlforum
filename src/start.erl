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
	User1 = #user{name="noah", id = 1, is_admin = true},
	User2 = #user{name="nate", id = 2, is_admin = false},
	Post1 = #post{user = User1, id = 1, contents = "hello, forum", tid = 1, is_deleted = false},
	Post2 = #post{user = User2, id = 2, contents = "hi, noah, welcome to the forum.", tid = 1, is_deleted = false },
	Thread = #thread{name="Hi", user=User1, posts = [Post1], id=1},
	user:add_user(Ref, User1),
	user:add_user(Ref, User2),
	thread:add_thread(Ref, Thread),
	post:add_post(Ref, Post2)
	.
