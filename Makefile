.PHONY: compile eunit

SRCS:=${shell ls src/*.erl src/*.app.src 2>/dev/null}
BEAMS=${patsubst test/%,ebin/%,${patsubst src/%,ebin/%,${SRCS}}}
BEAMS:=${patsubst %.app.src,%.app,${BEAMS}}
BEAMS:=${patsubst %.erl,%.beam,${BEAMS}}

TEST_SRCS:=${shell ls test/*.erl test/fixtures/*.erl 2>/dev/null}
TEST_BEAMS=${patsubst test/%,test/ebin/%,${TEST_SRCS}}
TEST_BEAMS:=${patsubst %.erl,%.beam,${TEST_BEAMS}}

compile: ${BEAMS}

clean:
	rm ebin/*.beam test/ebin/*.beam

ebin/%.beam: src/%.erl src/*.hrl
	erlc +debug_info -Werror -o ebin $<

test/ebin/%.beam: test/%.erl src/*.hrl
	erlc +debug_info -Werror -I src/ -o test/ebin $<

eunit: compile ${TEST_BEAMS}
	./run_tests test/*.erl

dialyzer.plt:
	dialyzer --build_plt --output_plt dialyzer.plt --apps compiler dialyzer erts kernel runtime_tools stdlib syntax_tools test_server tools common_test

dialyzer: compile dialyzer.plt
	dialyzer --plt dialyzer.plt ebin/*.beam
