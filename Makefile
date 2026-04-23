ISSUE ?= B
NUM_CORE ?= 2
NUM_TL_UL ?= 0
NUM_SLICE ?= 4
WITH_CHISELDB ?= 1
WITH_TLLOG ?= 1
WITH_CHILOG ?= 1
BY_ETIME ?= 1
BY_VTIME ?= 0
FPGA ?= 0

TEST_CCFG ?= 0

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i CoupledL2.compile

CHI_PASS_ARGS = ISSUE=$(ISSUE) NUM_CORE=$(NUM_CORE) NUM_TL_UL=$(NUM_TL_UL) NUM_SLICE=$(NUM_SLICE) \
			    WITH_CHISELDB=$(WITH_CHISELDB) WITH_TLLOG=$(WITH_TLLOG) WITH_CHILOG=$(WITH_CHILOG) \
				BY_ETIME=$(BY_ETIME) BY_VTIME=$(BY_VTIME) \
			    FPGA=$(FPGA)

TOP = TestTop
CHI_TOP_ARGS = --issue $(ISSUE) --core $(NUM_CORE) --tl-ul $(NUM_TL_UL) --bank $(NUM_SLICE) \
		   	   --chiseldb $(WITH_CHISELDB) --tllog $(WITH_TLLOG) --chilog $(WITH_CHILOG) \
			   --etime $(BY_ETIME) --vtime $(BY_VTIME) \
		       --fpga $(FPGA)
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).sv
MEM_GEN = ./scripts/vlsi_mem_gen
MEM_GEN_SEP = ./scripts/gen_sep_mem.sh

TEST_L2TSHRAlloc_TOP_ARGS = --ccfg $(TEST_CCFG)

gen-test-top:
	mill -i CoupledL2.test.runMain coupledL2.$(TOP)_$(SYSTEM) -td $(BUILD_DIR) --target systemverilog --split-verilog
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V).conf" "$(BUILD_DIR)"

gen-test-top-chi:
	mill -i CoupledL2.test.runMain coupledL2.$(TOP)_$(SYSTEM) -td $(BUILD_DIR) $(CHI_TOP_ARGS) --target systemverilog --split-verilog
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V).conf" "$(BUILD_DIR)"

test-top-l2:
	$(MAKE) gen-test-top SYSTEM=L2

test-top-l2standalone:
	$(MAKE) gen-test-top SYSTEM=L2_Standalone

test-top-l2l3:
	$(MAKE) gen-test-top SYSTEM=L2L3

test-top-l2l3l2:
	$(MAKE) gen-test-top SYSTEM=L2L3L2

test-top-fullsys:
	$(MAKE) gen-test-top SYSTEM=fullSys

test-top-chi:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS)

test-top-chi-dualcore-0ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=2 NUM_TL_UL=0

test-top-chi-dualcore-2ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=2 NUM_TL_UL=2

test-top-chi-quadcore-0ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=4 NUM_TL_UL=0

test-top-chi-quadcore-2ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=4 NUM_TL_UL=2

ut-sv-oceanus-L2TSHRAlloc:
	mill -i CoupledL2.test.runMain oceanus.TestTop_L2TSHRAlloc -td $(BUILD_DIR) $(TEST_L2TSHRAlloc_TOP_ARGS) --target systemverilog --split-verilog

ut-cc-oceanus-L2TSHRAlloc: ut-sv-oceanus-L2TSHRAlloc
	verilator ./build/*.*v --Mdir ./verilated --cc --top TestTop_L2TSHRAlloc --trace-vcd

ut-exe-oceanus-L2TSHRAlloc: ut-sv-oceanus-L2TSHRAlloc
	verilator ./build/*.*v --Mdir ./verilated --cc --top TestTop_L2TSHRAlloc --trace-vcd --build --exe \
		src/test/cpp/L2TSHRAlloc_v3main.cpp -I./build -I./src/test/cpp -o vt_L2TSHRAlloc -j `nproc` -CFLAGS "-g -O0"


clean:
	rm -rf ./build
	rm -rf ./verilated

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

.PHONY: init bsp checkformat clean compile idea reformat 