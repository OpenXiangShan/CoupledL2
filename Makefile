init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat api-config-chipsalliance

compile:
	mill -i CoupledL2.compile

test-top-l2:
	mkdir -p build
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2 -td build
	mv build/TestTop_L2.v build/TestTop.v

test-top-l2standalone:
	mkdir -p build
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2_Standalone -td build
	mv build/TestTop_L2_Standalone.v build/TestTop.v

test-top-l2l3:
	mkdir -p build
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2L3 -td build
	mv build/TestTop_L2L3.v build/TestTop.v

test-top-l2l3l2:
	mkdir -p build
	mill -i CoupledL2.test.runMain coupledL2.TestTop_L2L3L2 -td build
	mv build/TestTop_L2L3L2.v build/TestTop.v

test-top-fullsys:
	mkdir -p build
	mill -i CoupledL2.test.runMain coupledL2.TestTop_fullSys -td build
	mv build/TestTop_fullSys.v build/TestTop.v

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
