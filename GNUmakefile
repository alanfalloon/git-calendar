all:

CABAL=cabal
CABAL_CONFIGURE_FLAGS=--user --prefix=$(PREFIX)
PREFIX=$(HOME)/local

EXEC=dist/build/git-calendar/git-calendar
CONFIG=dist/setup-config

$(EXEC): force $(CONFIG)
	$(CABAL) build $(CABAL_BUILD_FLAGS)

$(CONFIG): Setup.hs git-calendar.cabal
	$(CABAL) configure $(CABAL_CONFIGURE_FLAGS)

force:: ;

test :: $(EXEC)
	$< --help
	$< --version
	cd ~/git && $(abspath $<) */.git/ */*/.git/

clean::
	rm -r dist

all:    $(EXEC)

.PHONY: all clean test
