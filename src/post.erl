-module(post).
-include("include/post.hrl").
-include("include/user.hrl").
-include("include/thread.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([shorten/1, selected2post/2, selected2posts/2, creat_post_table/1, add_post/3, get_post/2, render_post/1, render_posts/1]).

creat_post_table(Ref) ->
	odbc:sql_query(Ref, "CREATE TABLE posts(id INTEGER PRIMARY KEY AUTOINCREMENT, is_deleted BOOL, uid INTEGER, contents TEXT, tid INTEGER);").

add_post(Ref, Post, Thread) when is_record(Thread, thread) ->
	add_post(Ref, Post, Thread#thread.id) ;

add_post(Ref, Post, Tid) ->
	#post{user=User, contents=Contents} = Post,
	#user{id=Uid} = User,
	Uid2 = {sql_integer, [Uid]},
	Contents2 = {{sql_char, length(Contents)}, [Contents]},
	Tid2 = {sql_integer, [Tid]},
	odbc:param_query(Ref, "INSERT INTO posts (is_deleted, uid, contents, tid) VALUES(false, ?, ?, ?);" , [Uid2, Contents2, Tid2]).

selected2post(Ref, {Pid, IsDeleted, Uid, Contents, Tid}) -> #post{user=user:get_user(Ref, Uid), id=Pid, is_deleted=IsDeleted, contents=Contents, tid=Tid}.

selected2posts(_, []) -> [] ;
selected2posts(Ref, [Head|Tail]) -> [selected2post(Ref, Head)|selected2posts(Ref, Tail)].

get_post(Ref, Id) ->
	{selected, _, [Selected]} = odbc:param_query(Ref, "SELECT * FROM posts WHERE id == ?;", [{sql_integer, [Id]}]),
	selected2post(Ref, Selected).

add_post_test() ->
	odbc:start(),
	{ok, Ref} = odbc:connect("DSN=erlforum", []),
	user:creat_user_table(Ref),
	creat_post_table(Ref),
	User1 = #user{id = 1, name="noah", is_admin=true},
	Post1 = #post{user=User1, id=1, contents="hi", tid=1, is_deleted=false},
	user:add_user(Ref, User1),
	add_post(Ref, Post1, 1),
	Post2 = get_post(Ref, 1),
	?assert(Post1 =:= Post2),
	odbc:disconnect(Ref),
	odbc:stop(),
	file:delete("db/database.db").

render_post(Post) -> 
	#post{user = User, contents = Contents} = Post,
	"<div><table><tr><td bgcolor=\"red\">" ++ user:render_user(User) ++ "</td><td bgcolor=\"lightblue\"><div>" ++ Contents ++ "</div></td></tr></table></div>".

render_posts([]) -> "" ;
render_posts([Head|Tail]) ->
	render_post(Head) ++ "<br/>" ++ render_posts(Tail).

shorten(Post) when is_record(Post, post) ->
	shorten(Post#post.contents);

shorten(Post) when is_list(Post) ->
	if
		length(Post) < 140 ->
			Post ;
		true ->
			{X, _} = list:split(140, Post),
			X
	end.
