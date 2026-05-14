ROOT := ../../..
TB := L2TSHRAlloc_tb
TB_SV := $(TB).sv
VERILATOR_MAIN := verilator_main.cpp
RTL_FILELIST := $(ROOT)/build/filelist.f
RTL_SRCS ?= $(wildcard $(ROOT)/build/*.v) $(wildcard $(ROOT)/build/*.sv)

SIM ?= verilator
RUN_ARGS ?=
COVERAGE ?= 0
COVERAGE_ENABLE := $(filter 1 true yes on,$(COVERAGE))

VERILATOR ?= verilator
VERILATOR_TIMING ?= --timing
VERILATOR_FLAGS ?= $(VERILATOR_TIMING) --trace --top-module $(TB) -Wno-TIMESCALEMOD -I$(ROOT)/build
VERILATOR_COVERAGE_FLAGS ?= --coverage
VERILATOR_COVERAGE_FILE ?= $(TB).coverage.dat
VERILATOR_COVERAGE_ARGS := $(if $(COVERAGE_ENABLE),$(VERILATOR_COVERAGE_FLAGS))
VERILATOR_RUN_COVERAGE_ARGS := $(if $(COVERAGE_ENABLE),+verilator+coverage+file+$(VERILATOR_COVERAGE_FILE))
VERILATOR_CFLAGS ?= -std=gnu++20
VERILATOR_MAKEFLAGS ?=
VERILATOR_MAKEFLAGS_ARG := $(if $(strip $(VERILATOR_MAKEFLAGS)),-MAKEFLAGS "$(VERILATOR_MAKEFLAGS)")
VERILATOR_OUT ?= obj_dir_$(TB)
VERILATOR_BIN ?= V$(TB)

VCS ?= vcs
VCS_FLAGS ?= -full64 -sverilog -timescale=1ns/1ps -top $(TB) +define+VCS +incdir+$(ROOT)/build -debug_access+all
VCS_COVERAGE_METRICS ?= line+cond+fsm+tgl+branch
VCS_COVERAGE_DIR ?= $(TB).vdb
VCS_COVERAGE_NAME ?= $(TB)
VCS_COVERAGE_COMPILE_ARGS := $(if $(COVERAGE_ENABLE),-cm $(VCS_COVERAGE_METRICS))
VCS_COVERAGE_RUN_ARGS := $(if $(COVERAGE_ENABLE),-cm $(VCS_COVERAGE_METRICS) -cm_name $(VCS_COVERAGE_NAME) -cm_dir $(VCS_COVERAGE_DIR))
VCS_EXE ?= simv_$(TB)

.PHONY: all build run clean help
.PHONY: verilator-build verilator-run vcs-build vcs-run

all: build

build:
	@if [ "$(SIM)" = "verilator" ]; then \
		$(MAKE) verilator-build; \
	elif [ "$(SIM)" = "vcs" ]; then \
		$(MAKE) vcs-build; \
	else \
		echo "Unsupported SIM=$(SIM), expected 'verilator' or 'vcs'"; \
		exit 1; \
	fi

run:
	@if [ "$(SIM)" = "verilator" ]; then \
		$(MAKE) verilator-run; \
	elif [ "$(SIM)" = "vcs" ]; then \
		$(MAKE) vcs-run; \
	else \
		echo "Unsupported SIM=$(SIM), expected 'verilator' or 'vcs'"; \
		exit 1; \
	fi

verilator-build:
	$(VERILATOR) --cc --exe --build $(VERILATOR_FLAGS) $(VERILATOR_COVERAGE_ARGS) --Mdir $(VERILATOR_OUT) -CFLAGS "$(VERILATOR_CFLAGS)" $(VERILATOR_MAKEFLAGS_ARG) $(RTL_SRCS) $(TB_SV) $(VERILATOR_MAIN) -o $(VERILATOR_BIN)

verilator-run: verilator-build
	./$(VERILATOR_OUT)/$(VERILATOR_BIN) $(VERILATOR_RUN_COVERAGE_ARGS) $(RUN_ARGS)

vcs-build:
	$(VCS) $(VCS_FLAGS) $(VCS_COVERAGE_COMPILE_ARGS) $(RTL_SRCS) $(TB_SV) -o $(VCS_EXE)

vcs-run: vcs-build
	./$(VCS_EXE) $(VCS_COVERAGE_RUN_ARGS) $(RUN_ARGS)

clean:
	rm -rf $(VERILATOR_OUT) $(VCS_EXE) simv* csrc *.daidir *.vcd *.vpd *.vpd.vcd *.coverage.dat coverage.dat *.vdb

help:
	@echo "Usage:"
	@echo "  make build SIM=verilator   # Build with Verilator"
	@echo "  make run   SIM=verilator   # Build and run with Verilator"
	@echo "  make run   SIM=verilator RUN_ARGS='+cycles=1000 +seed=1'"
	@echo "  make run   SIM=verilator COVERAGE=1 RUN_ARGS='+cycles=1000'"
	@echo "  make run   SIM=verilator VERILATOR_MAKEFLAGS='CXX=clang++'"
	@echo "  make run   SIM=verilator RTL_SRCS='../../../build/*.sv'"
	@echo "  make build SIM=vcs         # Build with VCS"
	@echo "  make run   SIM=vcs         # Build and run with VCS"
	@echo "  make run   SIM=vcs RUN_ARGS='+cycles=1000 +seed=1'"
	@echo "  make run   SIM=vcs COVERAGE=1 RUN_ARGS='+cycles=1000'"
