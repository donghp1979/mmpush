.SUFFIXES: .erl .beam

%.beam:src/%.erl
	erlc -W $<

MODS = test test_client uuid mm_acceptor mm_switch  

ERL = erl -s mm_switch setup_test

all: compile

compile: ${MODS:%=%.beam}


run:all
	${ERL}


# application:
#     ${ERL} -s application start ARG1 ARG2


clean:
	rm -rf *.beam erl_crash.dump
