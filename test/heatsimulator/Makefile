.PHONY: all clean
all: HeatSimulator_cuda HeatSimulator_cpu1 HeatSimulator_cpu2 HeatSimulator_cpu4 HeatSimulator_cpu6

GCC = g++ -O2
NVCC = nvcc -O2

clean:
	rm -f HeatSimulator_cpu* HeatSimulator_cuda HeatSimulator.cu HeatSimulator*.cc

HeatSimulator.cu : ../../main.native ../../example/HeatSimulator.xfrp
	(cd ../.. && ./main.native -gpu true example/HeatSimulator.xfrp > test/heatsimulator/HeatSimulator.cu && cd test/heatsimulator)

HeatSimulator1.cc : ../../main.native ../../example/HeatSimulator.xfrp
	(cd ../.. && ./main.native example/HeatSimulator.xfrp > test/heatsimulator/HeatSimulator1.cc && cd test/heatsimulator)

HeatSimulator2.cc : ../../main.native ../../example/HeatSimulator.xfrp
	(cd ../.. && ./main.native -thread 2 example/HeatSimulator.xfrp > test/heatsimulator/HeatSimulator2.cc && cd test/heatsimulator)

HeatSimulator4.cc : ../../main.native ../../example/HeatSimulator.xfrp
	(cd ../.. && ./main.native -thread 4 example/HeatSimulator.xfrp > test/heatsimulator/HeatSimulator4.cc && cd test/heatsimulator)

HeatSimulator6.cc : ../../main.native ../../example/HeatSimulator.xfrp
	(cd ../.. && ./main.native -thread 6 example/HeatSimulator.xfrp > test/heatsimulator/HeatSimulator6.cc && cd test/heatsimulator)

HeatSimulator_cuda : HeatSimulator.cu setting.h
	$(NVCC) -o HeatSimulator_cuda -I/usr/local/cuda/samples/common/inc -DXFRP_ON_PTHREAD HeatSimulator.cu

HeatSimulator_cpu1 : HeatSimulator1.cc setting.h
	$(GCC) -o HeatSimulator_cpu1 -DXFRP_ON_PTHREAD HeatSimulator1.cc

HeatSimulator_cpu2 : HeatSimulator2.cc setting.h
	$(GCC) -o HeatSimulator_cpu2 -pthread -DXFRP_ON_PTHREAD HeatSimulator2.cc

HeatSimulator_cpu4 : HeatSimulator4.cc setting.h
	$(GCC) -o HeatSimulator_cpu4 -pthread -DXFRP_ON_PTHREAD HeatSimulator4.cc

HeatSimulator_cpu6 : HeatSimulator6.cc setting.h
	$(GCC) -o HeatSimulator_cpu6 -pthread -DXFRP_ON_PTHREAD HeatSimulator6.cc
