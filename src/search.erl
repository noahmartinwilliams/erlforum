-module(search).

-include("user.hrl").
-include("post.hrl").
-include("thread.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([search_all_threads/2, render_search_results/1]).

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
	?assert(split_query("hello world") =:= ["hello", "world"]),
	?assert(split_query("double post!") =:= ["double", "post!"]).

find_word(_, []) -> false ;
find_word(Word, [Head|_]) when Head =:= Word -> true ;
find_word(Word, [_|Tail]) -> find_word(Word, Tail).

search_text(Word, Text) ->
	Text2 = split_query(Text),
	find_word(Word, Text2).

search_text_test() ->
	?assert(search_text("quizzaciously", "\"Hey, Vsauce! Michael Here, but who is Michael and how much does here weigh?\" the great parodist said quizzaciously")).

search_texts(Words, Text) ->
	X = lists:map(fun(Head) -> search_text(Head, Text) end , Words),
	lists:foldr(fun(A, B) -> A or B end, false, X).

search_texts_test() ->
	?assert(search_texts(["hey", "vsauce"], "hey , vsauce ! Michael here!")),
	?assert(search_texts(["hi", "vsauce"], "hey , Michael here!") =:= false),
	?assert(search_texts(["world"], "world")).

search_post(Words, Post) ->
	search_texts(Words, Post#post.contents).

search_thread(_, []) -> true ;
search_thread(Words, [Post|Tail]) ->
	X = search_post(Words, Post),
	X and search_thread(Words, Tail).


search_threads(_, []) -> [] ;
search_threads(Words, [Head|Tail]) ->
	Results0 = search_thread(Words, Head#thread.posts),
	Results1 = search_texts(Words, Head#thread.name),
	Results = Results0 or Results1,
	if
		Results ->
			[Head|search_threads(Words, Tail)];
		true ->
			search_threads(Words, Tail)
	end.

search_all_threads(Ref, Query) ->
	Query2 = split_query(Query),
	X = thread:get_all_threads(Ref),
	search_threads(Query2, X).

render_search_results([]) -> "" ;
render_search_results([Head|Tail]) ->
	thread:render_thread_head(Head) ++ render_search_results(Tail).
