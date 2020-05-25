ERLC=erlc -I ../include/ -pz ../ebin/

main: ebin/cookie.beam ebin/user.beam ebin/post.beam ebin/thread.beam ebin/start.beam ebin/search.beam
	echo "done"

ebin/cookie.beam: src/cookie.erl
	cd src ; $(ERLC) cookie.erl ; cd .. ; mv src/cookie.beam ebin/

ebin/user.beam: src/user.erl
	cd src ; $(ERLC) user.erl ; cd .. ; mv src/user.beam ebin/

ebin/post.beam: src/post.erl
	cd src ; $(ERLC) post.erl ; cd .. ; mv src/post.beam ebin/

ebin/thread.beam: src/thread.erl
	cd src ; $(ERLC) thread.erl ; cd .. ; mv src/thread.beam ebin/

ebin/start.beam: src/start.erl
	cd src ; $(ERLC) start.erl ; cd .. ; mv src/start.beam ebin/

ebin/search.beam: src/search.erl
	cd src ; $(ERLC) search.erl ; cd .. ; mv src/search.beam ebin/
