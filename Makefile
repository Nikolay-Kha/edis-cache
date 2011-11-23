ERL := erl -pa deps/*/ebin -pa ebin -pa src -s crypto -boot start_sasl +Bc +K true -smp enable -s inets -s ssl -s elog ${ERL_ARGS}

all:
	rebar get-deps && rebar compile
	
clean:
	rebar clean

build_plt: all
	dialyzer --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
				    xmerl webtool snmp public_key mnesia eunit syntax_tools compiler --output_plt ~/.edis_plt -pa deps/*/ebin ebin

analyze: all
	dialyzer -pa deps/*/ebin --plt ~/.itweet_dialyzer_plt -Wunmatched_returns -Werror_handling -Wbehaviours ebin

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
run: all
	${ERL} -s edis

test: all
	${ERL} -config test.config -noshell -sname edis_test_server -s edis -run elog debug & 
	rebar skip_deps=true ct ; \
	kill `ps aux | grep beam | grep edis_[t]est_server | awk '{print $$2}'`

shell: all
	${ERL}
	
doc: all
	cd deps/erldocs 
	make
	cd ../..
	./deps/erldocs/erldocs doc
	cat doc/erldocs_index.js | tr -d '\n' > doc/erldocs_index2.js
	mv ./doc/erldocs_index2.js ./doc/erldocs_index.js
	
	