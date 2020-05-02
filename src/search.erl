-module(search).

-include("include/user.hrl").
-include("include/post.hrl").
-include("include/thread.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([search_all_threads/2]).

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

search_texts_receive(0) -> false ;
search_texts_receive(X) ->
	receive
		Y ->
			Y or search_texts_receive(X-1)
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
	?assert(search_texts(["hi", "vsauce"], "hey , Michael here!") =:= false).

search_post(Words, Post) ->
	search_texts(Words, Post#post.contents).

search_thread_worker(Words, Post, Pid) -> Pid ! search_post(Words, Post).

search_thread_receive(0) -> true ;
search_thread_receive(X) ->
	receive
		Y ->
			Y and search_thread_receive(X - 1)
	end.

search_thread_spawn([], _) -> 0 ;
search_thread_spawn([Post|Tail], Query) ->
	X = self(),
	link(spawn(fun() -> search_thread_worker(Query, Post, X) end)),
	1 + search_thread_spawn(Tail, Query).

search_thread(Words, Thread) ->
	Spawned = search_thread_spawn(Thread#thread.posts, Words),
	search_thread_receive(Spawned).

search_threads_worker(Words, Thread, Pid) -> 
	Results = search_thread(Words, Thread),
	if 
		Results ->
			Pid ! Thread ;
		true ->
			Pid ! dud 
	end.

search_threads_receive(0) -> [] ;
search_threads_receive(X) ->
	receive
		dud ->
			search_threads_receive(X-1) ;
		Y ->
			[Y|search_threads_receive(X-1)]
	end.

search_threads_spawn([], _ ) -> 0 ;
search_threads_spawn([Thread|Rest], Words) ->
	X = self(),
	link(spawn(fun() -> search_threads_worker(Words, Thread, X) end)),
	search_threads_spawn(Rest, Words).

search_threads(Words, Threads) ->
	Spawned = search_threads_spawn(Threads, Words),
	search_threads_receive(Spawned).

search_all_threads(Ref, Query) ->
	Query2 = split_query(Query),
	X = thread:get_all_threads(Ref),
	search_threads(Query2, X).
