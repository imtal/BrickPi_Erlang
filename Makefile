#  File:	 Makefile
#  Author:	 imtal@yolt.nl
#  Created:	 Sat 27 dec 2013 18:08

DESTDIR=/usr/lib
APPLICATION=brickpi
VSN = 0.8
INSTALL_DIR=brickpi-$(VSN)
FULL_INSTALL_DIR=$(DESTDIR)/erlang/lib/$(INSTALL_DIR)

DOC_OPTS = {dir,\"./doc\"}
WARNING_OPTIONS =
LANGUAGE_OPTIONS = 
COMPILER_OPTIONS = -g

CFLAGS   = $(WARNING_OPTIONS) $(LANGUAGE_OPTIONS) $(COMPILER_OPTIONS)

H_DIR = c_src
C_DIR = c_src
O_DIR = c_src

######################################################################

H_FILES = $(wildcard $(H_DIR)/*.h)
C_FILES = $(C_DIR)/brickpi_driver.c
#O_FILES = $(O_DIR)/brickpi_driver.o
O_FILES = $(patsubst $(C_DIR)/%.c, $(O_DIR)/%.o, $(C_FILES))

######################################################################

ERL_FILES = $(wildcard src/*.erl)
HRL_FILES = $(wildcard include/*.hrl)
BEAM_FILES = $(patsubst src/%.erl, ebin/%.beam, $(ERL_FILES))

######################################################################

all: priv/bin/brickpi_driver $(BEAM_FILES)

install: all
	@[ -n "$(DESTDIR)" ] || (echo "Set DESTDIR before running the install target."; false)
	install -d $(FULL_INSTALL_DIR)/ebin
	install -d $(FULL_INSTALL_DIR)/priv/bin
	install -d $(FULL_INSTALL_DIR)/src
	install -m 644 ebin/* $(FULL_INSTALL_DIR)/ebin
	install -m 755 priv/bin/* $(FULL_INSTALL_DIR)/priv/bin
	install -m 644 src/* $(FULL_INSTALL_DIR)/src

ebin:
	mkdir -p ebin

$(BEAM_FILES): $(ERL_FILES) $(HRL_FILES) ebin
	erlc -I include -o ebin $<

priv:
	mkdir -p priv

priv/bin: priv
	mkdir -p priv/bin

$(O_FILES): $(C_FILES) $(H_FILES)
	$(CC) -c -o $@ $< $(CFLAGS)

priv/bin/brickpi_driver: $(O_FILES) priv/bin
	$(CC) -o $@ $(LDFLAGS) $(O_FILES) $(LDLIBS) -lrt

# wirinPi not used anymore
#$(CC) -o $@ $(LDFLAGS) $(O_FILES) $(LDLIBS) -lrt -lm -L/usr/local/lib -lwiringPi

edoc:
	mkdir -p doc
	erl \
		-noshell \
		-pa ebin \
		-eval "edoc:application($(APPLICATION), \"./src\", [$(DOC_OPTS)])"\
		-s init stop

tests: all
	erlc -I include -o test test/testset.erl
	erl -pa test ebin -run testset -noshell
	@echo Ignored: python test/testset.py

.PHONY: clean

clean:
	rm -f  erl_crash.dump *.beam
	rm -f  priv/bin/brickpi_driver 
	rm -f $(O_FILES) 
	rm -f $(BEAM_FILES)
	rm -f  doc/*

#firmware: firmware_check
#	ino build
#	ino upload
#
#firmware_check:
#	@if [ -z "$(INO)" ]; then \
#		echo "\nNeed ino, see http://inotool.org\n\n"; exit 1; \
#	fi

info:
	@echo H_FILES = $(H_FILES)
	@echo O_FILES = $(O_FILES)

version:
	@echo $(VSN)

