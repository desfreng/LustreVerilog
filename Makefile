all: build

build:
	@cabal build

integ-test:
	@cabal run IntegTest

unit-test:
	@cabal test UnitTest --enable-coverage

open-coverage-report: unit-test
	@firefox $(shell find dist-newstyle/build -wholename "*/UnitTest/hpc_index.html")

open-unit-report: unit-test
	@firefox $(shell find dist-newstyle/build -name "*UnitTest.log")

open-report: open-coverage-report open-unit-report

run:
ifeq ($(ARGS),)
    @echo "Error: ARGS is not set"
else
	@cabal run LustreVerilog $(ARGS)
endif

clean:
	@cabal clean

repl:
	@cabal repl

.PHONY: build integ-test unit-test open-coverage-report open-unit-report open-report run clean repl