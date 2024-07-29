ISSUE ?= B

TEST_TOP_SUFFIX := UNKNOWN
ifeq ($(ISSUE), B)
TEST_TOP_SUFFIX :=
endif
ifeq ($(ISSUE), E.b)
TEST_TOP_SUFFIX := _Eb
endif

ifeq ($(TEST_TOP_SUFFIX), UNKNOWN)
$(error "Unknown CHI Issue specified: $(ISSUE)")
endif

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i CoupledL2.compile

TOP = TestTop
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SIM_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(TOP):-o:$(TOP).v.conf
MEM_GEN = ./scripts/vlsi_mem_gen
MEM_GEN_SEP = ./scripts/gen_sep_mem.sh

test-top:
	mill -i CoupledL2.test.runMain coupledL2.$(TOP)_$(SYSTEM) -td $(BUILD_DIR) $(SIM_MEM_ARGS)
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V).conf" "$(BUILD_DIR)"

test-top-l2:
	$(MAKE) test-top SYSTEM=L2

test-top-l2standalone:
	$(MAKE) test-top SYSTEM=L2_Standalone

test-top-l2l3:
	$(MAKE) test-top SYSTEM=L2L3

test-top-l2l3l2:
	$(MAKE) test-top SYSTEM=L2L3L2

test-top-fullsys:
	$(MAKE) test-top SYSTEM=fullSys

test-top-chi-dualcore-0ul:
	$(MAKE) test-top SYSTEM=CHI_DualCore_0UL$(TEST_TOP_SUFFIX)

test-top-chi-dualcore-2ul:
	$(MAKE) test-top SYSTEM=CHI_DualCore_2UL$(TEST_TOP_SUFFIX)

test-top-chi-quadcore-0ul:
	$(MAKE) test-top SYSTEM=CHI_QuadCore_0UL$(TEST_TOP_SUFFIX)

test-top-chi-quadcore-2ul:
	$(MAKE) test-top SYSTEM=CHI_QuadCore_2UL$(TEST_TOP_SUFFIX)

test-top-chi-octacore-0ul:
	$(MAKE) test-top SYSTEM=CHI_OctaCore_0UL$(TEST_TOP_SUFFIX)

test-top-chi-octacore-2ul:
	$(MAKE) test-top SYSTEM=CHI_OctaCore_2UL$(TEST_TOP_SUFFIX)

test-top-chi-hexacore-0ul:
	$(MAKE) test-top SYSTEM=CHI_HexaCore_0UL$(TEST_TOP_SUFFIX)

test-top-chi-hexacore-2ul:
	$(MAKE) test-top SYSTEM=CHI_HexaCore_2UL$(TEST_TOP_SUFFIX)

clean:
	rm -rf ./build

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

.PHONY: init bsp checkformat clean compile idea reformat 