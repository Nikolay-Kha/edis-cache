# edis Makefile
# Copyright (C) 2011 Electronic Inaka, LLC <contact at inakanetworks dot com>
# edis is licensed by Electronic Inaka, LLC under the Apache 2.0 license

ERL := erl -pa deps/*/ebin -pa ebin -pa src -boot start_sasl +Bc +K true -smp enable -s crypto -s inets -s ssl -s lager ${ERL_ARGS}
PREFIX= /usr/local
INSTALL_BIN= $(PREFIX)/bin
INSTALL= cp -p

all: erl
	mkdir -p bin
	./priv/script_builder

erl:
	rebar get-deps compile

clean:
	rm -rf bin
	rebar clean

build_plt: erl
	dialyzer --verbose --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
				    xmerl webtool snmp public_key mnesia eunit syntax_tools compiler --output_plt ~/.edis_plt -pa deps/*/ebin ebin

analyze: erl
	dialyzer --verbose -pa deps/*/ebin --plt ~/.edis_plt -Wunmatched_returns -Werror_handling -Wbehaviours ebin

xref: erl
	rebar skip_deps=true xref

check_hosts:
	if [ ! -f ~/.hosts.erlang ] ; then echo "file ~/.hosts.erlang does not exist, run 'touch ~/.hosts.erlang' to fix this problem" ; exit 1  ; fi

run:  erl check_hosts
	${ERL} -s edis

ets:  erl check_hosts
	${ERL} -s edis -config ets.config

riak: erl check_hosts
	${ERL} -s edis -config riak.config

cache: erl check_hosts
	${ERL} -s edis -config cache.config

test: erl
	${ERL} -config test/test.config -noshell -sname edis_test_server -s edis &
	mkdir -p ./test/ebin
	mkdir -p ./logs/ct
	# To run specified tests, add suites=keys
	# rebar skip_deps=true ct suites=keys -v;
	# where keys is one of _SUITE.erl from test dir
	rebar skip_deps=true ct -v;
	#kill `ps aux | grep beam | grep edis_[t]est_server | awk '{print $$2}'`

test-riak: erl
	${ERL} -config test/test-riak.config -noshell -sname edis_test_server -s edis &
	mkdir -p ./test/ebin
	mkdir -p ./logs/ct
	rebar skip_deps=true ct -v;

test-cache: erl
	${ERL} -config test/test-cache.config -noshell -sname edis_test_server -s edis &
	mkdir -p ./test/ebin
	mkdir -p ./logs/ct
	rebar skip_deps=true ct -v;

test-hanoidb: erl
	${ERL} -config test/test-hanoidb.config -noshell -sname edis_test_server -s edis -run elog debug &
	mkdir -p ./test/ebin
	erlc -o ./test/ebin -pa deps/lager/ebin +debug_info +'{parse_transform, lager_transform}' ./test/*_SUITE.erl
	rebar skip_deps=true ct ; \
	kill `ps aux | grep beam | grep edis_[t]est_server | awk '{print $$2}'`

shell: erl
	${ERL}

doc: erl
	rebar skip_deps=true doc

install:
	mkdir -p $(INSTALL_BIN)
	$(INSTALL) bin/* $(INSTALL_BIN)

service: install
	mkdir -p /etc/edis/db/
	if [ ! -f /etc/edis/edis.config ] ; then cp priv/edis.config /etc/edis/ ; fi
	cp priv/edis.init.d /etc/init.d/edis

