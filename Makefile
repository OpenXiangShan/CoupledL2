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

test-top-l2:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2 -td build

test-top-l2standalone:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2_Standalone -td build

test-top-l2l3:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2L3 -td build

test-top-l2l3l2:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2L3L2 -td build

test-top-fullsys:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_fullSys -td build

test-top-chi-dualcore-0ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_DualCore_0UL$(TEST_TOP_SUFFIX) -td build

test-top-chi-dualcore-2ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_DualCore_2UL$(TEST_TOP_SUFFIX) -td build

test-top-chi-quadcore-0ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_QuadCore_0UL$(TEST_TOP_SUFFIX) -td build

test-top-chi-quadcore-2ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_QuadCore_2UL$(TEST_TOP_SUFFIX) -td build

test-top-chi-octacore-0ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_OctaCore_0UL$(TEST_TOP_SUFFIX) -td build

test-top-chi-octacore-2ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_OctaCore_2UL$(TEST_TOP_SUFFIX) -td build

test-top-chi-hexacore-0ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_HexaCore_0UL$(TEST_TOP_SUFFIX) -td build

test-top-chi-hexacore-2ul:
	mill -i CoupledL2.test.runMain coupledL2.TestTop_CHI_HexaCore_2UL$(TEST_TOP_SUFFIX) -td build

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
