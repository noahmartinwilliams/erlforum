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

find_word(_, []) -> false ;
find_word(Word, [Head|_]) when Head =:= Word -> true ;
find_word(Word, [_|Tail]) -> find_word(Word, Tail).

search_text(Word, Text) ->
	Text2 = split_query(Text),
	find_word(Word, Text2).

search_text_test() ->
	?assert(search_text("quizzaciously", "\"Hey, Vsauce! Michael Here, but who is Michael and how much does here weigh?\" the great parodist said quizzaciously")).

search_texts_worker(Word, Text, Pid) -> Pid ! search_text(Word, Text).

search_texts_receive(0) -> true ;
search_texts_receive(X) ->
	receive
		Y ->
			Y and search_texts_receive(X-1)
	end.

search_texts_spawn([], _) -> 0 ;
search_texts_spawn([Head|Tail], Text) ->
	X = self(),
	link(spawn(fun() -> search_texts_worker(Head, Text, X) end)),
	1 + search_texts_spawn(Tail, Text).

search_texts(Words, Text) ->
	X = search_texts_spawn(Words, Text),
	search_texts_receive(X).

search_texts_test() ->
	?assert(search_texts(["hey", "vsauce"], "hey , vsauce ! Michael here!")),
	?assert(search_texts(["hey", "vsauce"], "hey , Michael here!") =:= false).
