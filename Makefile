.SUFFIXES: .erl .beam

%.beam:src/%.erl
	erlc -W $<

MODS = test

ERL = erl -s test print hello

all: compile

compile: ${MODS:%=%.beam}


run:all
	${ERL}


# application:
#     ${ERL} -s application start ARG1 ARG2


clean:
	rm -rf *.beam erl_crash.dump
