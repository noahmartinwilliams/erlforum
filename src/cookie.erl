-module(cookie).

-include("include/cookie.hrl").
-export([creat_cookie_table/1, add_cookie/2]).

creat_cookie_table(Ref) ->
	odbc:sql_query(Ref, "CREATE TABLE cookies(id INTEGER, str CHAR(140));").

add_cookie(Ref, Cookie) ->
	odbc:param_query(Ref, "INSERT INTO cookies (id, str) VALUES(?, ?);", [{sql_integer, [Cookie#choc_cookie.uid]}, {{sql_char, 140}, [Cookie#choc_cookie.chocolate_chip]}]).

