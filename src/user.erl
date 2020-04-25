-module(user).
-include("include/user.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([creat_user_table/1, verify_password/2, rand_string/1, add_user/3, get_user/2, render_user/1]).
creat_user_table(Ref) ->
	odbc:sql_query(Ref, "CREATE TABLE users(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE, is_admin BOOL, salt CHAR(32), hash CHAR(64) );").

rand_char() ->
	Chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
	X = random:uniform(length(Chars)),
	lists:nth( X , Chars ).

rand_string(0) -> "";
rand_string(X) -> [rand_char() | rand_string( X - 1)].

verify_password(User, Pass) ->
	#user{salt = Salt, hash = Hash} = User,
	<<X:256/big-unsigned-integer>> = crypto:hash(sha256, (Salt ++ Pass)),
	Y = lists:flatten(io_lib:format("~64.16.0b", [X])),
	Hash =:= Y.

add_user(Ref, User, Password) when is_record(User, user) ->
	#user{name=Name, is_admin=IsAdmin} = User,
	Salt = rand_string(32),
	<<X:256/big-unsigned-integer>> = crypto:hash(sha256, (Salt ++ Password)),
	Y = lists:flatten(io_lib:format("~64.16.0b", [X])),
	odbc:param_query(Ref, "INSERT INTO users (name, is_admin, salt, hash) VALUES(?, ?, ?, ?);", [{{sql_char, length(Name)}, [Name]}, {sql_bit, [IsAdmin]}, {{sql_char, length(Salt)}, [Salt]}, {{sql_char, length(Y)}, [Y]}]);

add_user(Ref, User, Password) when is_list(User) ->
	Salt = rand_string(32),
	<<X:256/big-unsigned-integer>> = crypto:hash(sha256, (Salt ++ Password)),
	Y = lists:flatten(io_lib:format("~64.16.0b", [X])),
	odbc:param_query(Ref, "INSERT INTO users (name, is_admin, salt, hash) VALUES(?, ?, ?, ?);", [{{sql_char, length(User)}, [User]}, {sql_bit, [false]}, {{sql_char, length(Salt)}, [Salt]}, {{sql_char, length(Y)}, [Y]}]).

get_user(Ref, Name) when is_list(Name) ->
	{selected, _, [{Id, _, IsAdmin, Salt, Hash}]} = odbc:param_query(Ref, "SELECT * FROM users WHERE name == ?;", [{{sql_char, length(Name)}, [Name]}]),
	#user{id=Id, name=Name, is_admin=IsAdmin, salt = Salt, hash = Hash};
get_user(Ref, Id) when is_integer(Id) ->
	{selected, _, [{_, Name, IsAdmin, Salt, Hash}]} = odbc:param_query(Ref, "SELECT * FROM users WHERE id == ?;", [{sql_integer, [Id]}]),
	#user{id=Id, name=Name, is_admin=IsAdmin, salt = Salt, hash = Hash}.

add_user_test() ->
	odbc:start(),
	{ok, Ref} = odbc:connect("DSN=erlforum", []),
	creat_user_table(Ref),
	User1 = #user{id = 1, name="noah", is_admin=true},
	add_user(Ref, User1, "password"),
	User2=get_user(Ref, "noah"),
	?assert(User2#user.name =:= User1#user.name),
	?assert(User2#user.id =:= User1#user.id),
	odbc:disconnect(Ref),
	odbc:stop(),
	file:delete("db/database.db").

verify_password_test() ->
	odbc:start(),
	{ok, Ref} = odbc:connect("DSN=erlforum", []),
	creat_user_table(Ref),
	User1 = #user{id = 1, name="nate", is_admin=true},
	add_user(Ref, User1, "password"),
	User2=get_user(Ref, "nate"),
	?assert(verify_password(User2, "password")),
	odbc:disconnect(Ref),
	odbc:stop(),
	file:delete("db/database.db").


render_user(User) when is_record(User, user) ->
	#user{name=Name} = User,
	"<div>" ++ Name ++ "</div>".
