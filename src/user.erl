-module(user).
-include("include/user.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([creat_user_table/1, add_user/2, get_user/2]).
creat_user_table(Ref) ->
	odbc:sql_query(Ref, "CREATE TABLE users(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE);").

add_user(Ref, User) when is_record(User, user) ->
	#user{name=Name} = User,
	odbc:param_query(Ref, "INSERT INTO users (name) VALUES(?);", [{{sql_char, length(Name)}, [Name]}]);

add_user(Ref, User) when is_list(User) ->
	odbc:param_query(Ref, "INSERT INTO users (name) VALUES(?);", [{{sql_char, length(User)}, [User]}]).

get_user(Ref, Name) when is_list(Name) ->
	{selected, _, [{Id, _}]} = odbc:param_query(Ref, "SELECT * FROM users WHERE name == ?;", [{{sql_char, length(Name)}, [Name]}]),
	#user{id=Id, name=Name};
get_user(Ref, Id) when is_integer(Id) ->
	{selected, _, [{_, Name}]} = odbc:param_query(Ref, "SELECT * FROM users WHERE id == ?;", [{sql_integer, [Id]}]),
	#user{id=Id, name=Name}.

add_user_test() ->
	odbc:start(),
	{ok, Ref} = odbc:connect("DSN=erlforum", []),
	creat_user_table(Ref),
	User1 = #user{id = 1, name="noah"},
	add_user(Ref, #user{id=1, name="noah"}),
	User2=get_user(Ref, "noah"),
	?assert(User2 =:= User1),
	odbc:disconnect(Ref),
	odbc:stop(),
	file:delete("db/database.db").

