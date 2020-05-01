-module(search).

-include("include/user.hrl").
-include("include/post.hrl").
-include("include/thread.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([split_query/1]).

split_query([]) -> [] ;
split_query([A]) when is_list(A) -> 
	[A] ;
split_query([A|Rest]) when is_list(A) =:= false ->
	split_query([[A]|Rest]) ;
split_query([A, B|Rest]) when is_list(A) and ( B =/= 32 )->
	split_query([A ++ [B] | Rest]) ;
split_query([A, B|Rest]) when is_list(A) and ( B =:= 32 ) -> % B =:= ' ' 
	[A|split_query(Rest)].

split_query_test() ->
	?assert(split_query("hello") =:= ["hello"]),
	?assert(split_query("hello ") =:= ["hello"]),
	?assert(split_query("hello world") =:= ["hello", "world"]).
