#leave these lines alone

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

# erlang modules need compiling
MODS = module1 module2 \
		module3 ... \
		moduleN

all: compile

compile: $(MODS:%=%.beam} subdirs

# 指出编译必须项并添加

special1.beam: special1.erl
	${ERL} -Dflag1 -W0 special1.erl

application1: compile
	${ERL} -pa Dir1 -s application1 start Arg1 Arg2

subdirs:
	cd dir1; make
	cd dir2; make
	...

clean:
	rm -rf *.beam erl_crash.dump
	cd dir1; make clean
	cd dir2; make clean
